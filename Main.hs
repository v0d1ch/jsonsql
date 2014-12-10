{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where
import Data.Aeson
import Data.Monoid
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.Encoding as T (decodeUtf8)
import Data.List (intersperse)
import qualified Data.List 
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import Data.Maybe (catMaybes)
import Control.Applicative
import Control.Monad (when)
import Data.ByteString.Lazy as BL hiding (map, intersperse)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Attoparsec.Lazy as LA hiding (Result, parseOnly)
import Data.Attoparsec.ByteString.Char8 (endOfLine, sepBy)
import Data.Attoparsec.Text 
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Data.Scientific 
import System.Environment (getArgs)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import qualified Options.Applicative as O
import Data.String.QQ
import Test.HUnit 

data Options = Options {  
      template :: Template
    } deriving Show

data Template = TemplateFile FilePath | TemplateText Text deriving (Show)

parseOpts :: O.Parser Options
parseOpts = Options <$> (tmplText <|> tmplFile)

tmplText = TemplateText . T.pack <$> O.argument O.str (O.metavar "TEMPLATE")
tmplFile = TemplateFile 
      <$> O.strOption (O.metavar "FILE" <> O.short 'f' <> O.help "Template file")

opts = O.info (O.helper <*> parseOpts)
          (O.fullDesc 
          <> O.progDesc "Inject JSON into SQL template strings" 
          <> O.header "jsonsql")

main = do
  Options tmpl <- O.execParser opts
  template <- case tmpl of
                  TemplateFile fp -> T.readFile fp
                  TemplateText t -> return t
  x <- BL.getContents 
  let vs :: [Value]
      vs = decodeStream x
      chunks :: [Chunk] 
      chunks = parseText template
      results = mconcat $ map (evalText chunks) vs
  TL.putStrLn . B.toLazyText $ results

decodeStream :: (FromJSON a) => BL.ByteString -> [a]
decodeStream bs = case decodeWith json bs of
    (Just x, xs) | xs == mempty -> [x]
    (Just x, xs) -> x:(decodeStream xs)
    (Nothing, _) -> []

decodeWith :: (FromJSON a) => LA.Parser Value -> BL.ByteString -> (Maybe a, BL.ByteString)
decodeWith p s =
    case LA.parse p s of
      LA.Done r v -> f v r
      LA.Fail _ _ _ -> (Nothing, mempty)
  where f v' r = (\x -> case x of 
                      Success a -> (Just a, r)
                      _ -> (Nothing, r)) $ fromJSON v'


data ArrayFormat = ArrayFormat {
    arrDelimiter :: Text
  , arrPrefixStr :: Text  -- can me mempty
  , arrPostFixStr :: Text
  } deriving (Show, Eq)

data Chunk = Pass Text | Expr KeyPath deriving (Show, Eq)
data KeyPath = KeyPath [Key] ArrayFormat deriving (Show, Eq) -- Text fields are array item prefix and postfix strings, which is given at end
data Key = Key Text | Index Int deriving (Eq, Show)

evalText :: [Chunk] -> Value -> B.Builder
evalText xs v = mconcat $ map (B.fromText . evalChunk v) xs

evalChunk :: Value -> Chunk -> Text
evalChunk v (Pass s) = s
evalChunk v (Expr k) = evalToText k v

evalToText :: KeyPath -> Value -> Text
evalToText k v = valToText $ evalKeyPath k v

evalToUnescapedText :: KeyPath -> Value -> Text
evalToUnescapedText k v = valToUnescapedText $ evalKeyPath k v

-- evaluates the a JS key path against a Value context to a leaf Value
evalKeyPath :: KeyPath -> Value -> Value
evalKeyPath (KeyPath [] _) x@(String _) = x
evalKeyPath (KeyPath [] _) x@Null = x
evalKeyPath (KeyPath [] _) x@(Number _) = x
evalKeyPath (KeyPath [] _) x@(Bool _) = x
evalKeyPath (KeyPath [] _) x@(Object _) = x
evalKeyPath (KeyPath (Key key:ks) a) (Object s) = 
    case (HM.lookup key s) of
        Just x          -> evalKeyPath (KeyPath ks a) x
        Nothing -> Null
evalKeyPath (KeyPath (Index idx:ks) a) (Array v) = 
      let e = (V.!?) v idx
      in case e of 
        Just e' -> evalKeyPath (KeyPath ks a) e'
        Nothing -> Null  
evalKeyPath k@(KeyPath _ ArrayFormat {..}) (Array v) = 
      let vs = V.toList v
          f =  (\v' -> escapeText $ arrPrefixStr <> evalToUnescapedText k v' <> arrPostFixStr)
          result = mconcat . intersperse arrDelimiter $ map f vs 
      in if result == mempty then Null else (String result)
evalKeyPath (KeyPath (Index _:_) _ ) _ = Null
evalKeyPath _ _ = Null

valToUnescapedText :: Value -> Text
valToUnescapedText (String x) = x
valToUnescapedText x = valToText x

escapeText = T.pack . escapeStringLiteral . T.unpack 

valToText :: Value -> Text
valToText (String x) = T.singleton '\'' 
    <> (escapeText x)
    <> T.singleton '\''
valToText Null = "NULL"
valToText (Bool True) = "TRUE"
valToText (Bool False) = "FALSE"
valToText (Number x) = 
    case floatingOrInteger x of
        Left float -> T.pack . show $ float
        Right int -> T.pack . show $ int
valToText x@(Object _) = error $ "Cannot interpolate " ++ show x

escapeStringLiteral :: String -> String
escapeStringLiteral ('\'':xs) = '\'': ('\'' : escapeStringLiteral xs)
escapeStringLiteral (x:xs) = x : escapeStringLiteral xs
escapeStringLiteral [] = []

parseText :: Text -> [Chunk]
parseText = either error id . parseOnly (many textChunk)

textChunk = exprChunk <|> passChunk

identifierChar = inClass "a-zA-Z_.[]0-9"

exprChunk :: Parser Chunk
exprChunk = do
    try (char ':')
    x <- notChar ':'
    xs <- takeWhile1 identifierChar
    arrFormat <- pArrayFormat
    let kp = parseKeyPath (T.singleton x <> xs) arrFormat
    return $ Expr kp

passChunk :: Parser Chunk
passChunk = Pass <$> takeWhile1 (notInClass ":")

parseKeyPath :: Text -> ArrayFormat -> KeyPath
parseKeyPath s a = case parseOnly pKeys s of
    Left err -> error $ "Error parsing key path: " ++ T.unpack s ++ " error: " ++ err 
    Right res -> KeyPath res a

pKeys :: Parser [Key]
pKeys = do
    keys <- sepBy1 pKeyOrIndex (takeWhile1 $ inClass ".[") 
    return keys 

-- | syntax is {delimiter-string!prefix-string!postfix-string}
-- immediately after last key
-- e.g. {,!!}
pArrayFormat :: Parser ArrayFormat
pArrayFormat = do 
  try (do
       char '{'
       delimiter <- T.pack <$> manyTill anyChar (char '!')
       pre <- T.pack <$> manyTill anyChar (char '!')
       post <- T.pack <$> manyTill anyChar (char '}')
       return $ ArrayFormat delimiter pre post)
  <|> pure defArrayFormat

defArrayFormat = ArrayFormat "," "" ""

pKeyOrIndex = pIndex <|> pKey

pKey = Key <$> takeWhile1 (notInClass " .[")

pIndex = Index <$> decimal <* char ']'

------------------------------------------------------------------------
-- Tests

runTests = runTestTT tests

tests = test [
    "testOne"          
        ~: []
        @=?   parseText "VALUES (:title, :year, :ratings.imdb)"
  , "test complex key"
        ~: []
        @=? parseText "values (:imdb, :versions.Rental.HD[0]);"
  ]


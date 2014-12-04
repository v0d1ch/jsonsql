{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Aeson
import Data.Monoid
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.Encoding as T (decodeUtf8)
import Data.List (intersperse)
import qualified Data.List 
import qualified Data.Text as T
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
import Test.HUnit 

data Options = Options deriving Show

parseOpts :: O.Parser Options
parseOpts = pure Options 

opts = O.info (O.helper <*> parseOpts)
          (O.fullDesc 
          <> O.progDesc "Inject JSON into SQL template strings" 
          <> O.header "jsonsql")

main = do
  Options <- O.execParser opts
  x <- BL.getContents 
  let xs :: [Value]
      xs = decodeStream x
  print xs      

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


data Chunk = Pass Text | Expr KeyPath deriving (Show, Eq)
type KeyPath = [Key]
data Key = Key Text | Index Int deriving (Eq, Show)

evalText :: Value -> Chunk -> Text
evalText v (Pass s) = s
evalText v (Expr ks) = eval ks v

eval :: KeyPath -> Value -> Text
eval ks v = valToText $ evalKeyPath ks v

-- evaluates the a JS key path against a Value context to a leaf Value
evalKeyPath :: KeyPath -> Value -> Value
evalKeyPath [] x@(String _) = x
evalKeyPath [] x@Null = x
evalKeyPath [] x@(Number _) = x
evalKeyPath [] x@(Bool _) = x
evalKeyPath [] x@(Object _) = x
evalKeyPath [] x@(Array v) = 
          let vs = V.toList v
              xs = intersperse "," $ map (eval []) vs
          in String . mconcat $ xs
evalKeyPath (Key key:ks) (Object s) = 
    case (HM.lookup key s) of
        Just x          -> evalKeyPath ks x
        Nothing -> Null
evalKeyPath (Index idx:ks) (Array v) = 
      let e = (V.!?) v idx
      in case e of 
        Just e' -> evalKeyPath ks e'
        Nothing -> Null
-- traverse array elements with additional keys
evalKeyPath ks@(Key key:_) (Array v) = 
      let vs = V.toList v
      in String . mconcat . intersperse "," $ map (eval ks) vs
evalKeyPath ((Index _):_) _ = Null
evalKeyPath _ _ = Null

valToText :: Value -> Text
valToText (String x) = x
valToText Null = "null"
valToText (Bool True) = "t"
valToText (Bool False) = "f"
valToText (Number x) = 
    case floatingOrInteger x of
        Left float -> T.pack . show $ float
        Right int -> T.pack . show $ int
valToText (Object _) = "[Object]"

parseText :: Text -> [Chunk]
parseText = either error id . parseOnly (many textChunk)

textChunk = exprChunk <|> passChunk

identifierChar = inClass "a-zA-Z_."

exprChunk :: Parser Chunk
exprChunk = do
    try (char ':')
    x <- notChar ':'
    xs <- takeWhile1 identifierChar
    let kp = parseKeyPath $ T.singleton x <> xs
    return $ Expr kp

passChunk :: Parser Chunk
passChunk = Pass <$> takeWhile1 (notInClass ":")

parseKeyPath :: Text -> KeyPath
parseKeyPath s = case parseOnly pKeyPath s of
    Left err -> error $ "Error parsing key path: " ++ T.unpack s ++ " error: " ++ err 
    Right res -> res

pKeyPath :: Parser KeyPath
pKeyPath = sepBy1 pKeyOrIndex (takeWhile1 $ inClass ".[")

pKeyOrIndex = pIndex <|> pKey

pKey = Key <$> takeWhile1 (notInClass " .[")

pIndex = Index <$> decimal <* char ']'

------------------------------------------------------------------------
-- Tests

runTests = runTestTT tests

tests = test [
    "testOne"          
        ~: [Pass "VALUES (",Expr [Key "title"],Pass ", ",Expr [Key "year"],Pass ", ",Expr [Key "ratings",Key "imdb"],Pass ")"]
        @=?   parseText "VALUES (:title, :year, :ratings.imdb)"
  ]


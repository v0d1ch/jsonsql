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
import qualified Data.Attoparsec.Lazy as Atto hiding (Result, parseOnly)
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

decodeWith :: (FromJSON a) => Atto.Parser Value -> BL.ByteString -> (Maybe a, BL.ByteString)
decodeWith p s =
    case Atto.parse p s of
      Atto.Done r v -> f v r
      Atto.Fail _ _ _ -> (Nothing, mempty)
  where f v' r = (\x -> case x of 
                      Success a -> (Just a, r)
                      _ -> (Nothing, r)) $ fromJSON v'


data Chunk = Pass Text | Expr Text deriving (Show, Eq)

-- evalText :: Value -> Chunk -> String
-- evalText v (Pass s) = s
-- evalText v (Expr s) = ngEvalToString v s

-- | function to evaluate an ng-expression and a object value context
-- e.g. Value -> "item.name" -> "John"
-- ngEvalToString :: Value -> String -> String
-- ngEvalToString context exprString = valToString . ngExprEval (runParse ngExpr exprString) $ context

parseText :: Text -> [Chunk]
parseText = either error id . parseOnly (many textChunk)

textChunk = exprChunk <|> passChunk

identifierChar = inClass "a-zA-Z_."

exprChunk :: Parser Chunk
exprChunk = do
    try (char ':')
    x <- notChar ':'
    xs <- takeWhile1 identifierChar
    return $ Expr $ T.singleton x <> xs

passChunk :: Parser Chunk
passChunk = Pass <$> takeWhile1 (notInClass ":")

------------------------------------------------------------------------
-- Tests

runTests = runTestTT tests

tests = test [
    "testOne"          
        ~: [Pass "VALUES (",Expr "title",Pass ", "
           ,Expr "year",Pass ", ",Expr "ratings.imdb",Pass ")"]
        @=?   parseText "VALUES (:title, :year, :ratings.imdb)"
  ]


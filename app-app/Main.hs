module Main where

import           Control.Applicative
import           Data.Foldable       (asum)
import qualified Data.Map.Strict     as M
import           Text.Pretty.Simple  (pPrint)

import           Parser

data Token
  = OpenSq
  | CloseSq
  | OpenBr
  | CloseBr
  | Comma
  | Column
  | TokString String
  | TokNumber Int
  | TokBool Bool
  | TokNull
  deriving (Eq, Show, Ord)

toLitStr :: Token -> Maybe String
toLitStr (TokString s) = Just s
toLitStr _             = Nothing

toLitInt :: Token -> Maybe Int
toLitInt (TokNumber i) = Just i
toLitInt _             = Nothing

toLitBool :: Token -> Maybe Bool
toLitBool (TokBool b) = Just b
toLitBool _           = Nothing

data Value
  = JsonObject (M.Map String Value)
  | JsonArray [Value]
  | JsonString String
  | JsonBool Bool
  | JsonNumber Int
  | JsonNull
  deriving (Eq, Show)

singleton :: a -> [a]
singleton a = [a]

tokenTransformSeq :: [(Token, String)] -> Parser Char () Token
tokenTransformSeq = asum . fmap transform
  where
    transform (token, str) = token <$ traverseP str

tokenTransform :: [(Token, Char)] -> Parser Char () Token
tokenTransform = asum . fmap transform
  where
    transform (token, char) = token <$ parseOne char

lexer :: Parser Char () [Token]
lexer = mainParser <* wsParser <* parseEof
  where
    wsParser = many $ parseOne ' ' <|> parseOne '\n' <|> parseOne '\t'
    mainParser =
      many $
      wsParser *>
      (parseSymbol <|> parseString <|> parseNumber <|> parseNull <|> parseBool)
    parseSymbol =
      tokenTransform
        [ (OpenSq, '[')
        , (CloseSq, ']')
        , (OpenBr, '{')
        , (CloseBr, '}')
        , (Comma, ',')
        , (Column, ':')
        ]
    parseString =
      TokString <$> (parseOne '"' *> parseWhileNe '"' <* parseOne '"')
    parseNumber =
      TokNumber . read <$>
      (singleton <$> parseRange '1' '9') <> many (parseRange '0' '9')
    parseNull = tokenTransformSeq [(TokNull, "null")]
    parseBool =
      tokenTransformSeq [(TokBool True, "true")] <|>
      tokenTransformSeq [(TokBool False, "false")]

parser :: Parser Token () Value
parser = value <* parseEof
  where
    value =
      jsonBool <|> jsonNull <|> jsonNumber <|> jsonObj <|> jsonArray <|>
      jsonString
    jsonBool = JsonBool <$> parseOptional toLitBool
    jsonString = JsonString <$> parseOptional toLitStr
    jsonNumber = JsonNumber <$> parseOptional toLitInt
    jsonArray =
      parseOne OpenSq *> (JsonArray <$> commaSep value) <* parseOne CloseSq
    jsonNull = JsonNull <$ parseOne TokNull
    jsonObj =
      parseOne OpenBr *> (JsonObject . M.fromList <$> commaSep kvPair) <*
      parseOne CloseBr
    kvPair = do
      label <- parseOptional toLitStr
      parseOne Column
      value' <- value
      return (label, value')
    commaSep parser = (singleton <$> parser) <> many (parseOne Comma *> parser)

frontend :: Parser Char () Value
frontend = lexer >$> parser

main :: IO ()
main = do
  programText <- readFile "input.json"
  case runParser frontend programText mempty of
    Left err           -> pPrint err
    Right (s, program) -> pPrint program

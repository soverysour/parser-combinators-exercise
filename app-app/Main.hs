module Main where

import           Control.Applicative
import           Data.Foldable       (asum)
import           Data.List           (intersperse)
import qualified Data.Map.Strict     as M
import           Text.Pretty.Simple  (pPrint)

import           Parser

{-
  Token - an enumeration for the possible token types of our parser.
-}
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

{-
  Function taking
    - A token to collect for String.
  Returns
    - A Maybe String, if the collection was successful.
-}
toLitStr :: Token -> Maybe String
toLitStr (TokString s) = Just s
toLitStr _             = Nothing

{-
  Function taking
    - A token to collect for Int.
  Returns
    - A Maybe Int, if the collection was successful.
-}
toLitInt :: Token -> Maybe Int
toLitInt (TokNumber i) = Just i
toLitInt _             = Nothing

{-
  Function taking
    - A token to collect for booleans.
  Returns
    - A Maybe Bool, if the collection was successful.
-}
toLitBool :: Token -> Maybe Bool
toLitBool (TokBool b) = Just b
toLitBool _           = Nothing

{-
  Here we describe our json type, an ADT.
-}
data Value
  = JsonObject (M.Map String Value)
  | JsonArray [Value]
  | JsonString String
  | JsonBool Bool
  | JsonNumber Int
  | JsonNull
  deriving (Eq, Show)

{-
  Function taking
    - A Value to be serialized
  Returns
    - A string = the serialized json.
-}
build :: Value -> String
build JsonNull = "null"
build (JsonNumber n) = show n
build (JsonBool b) =
  if b
    then "true"
    else "false"
build (JsonString s) = "\"" ++ s ++ "\""
build (JsonArray arr) = "[" ++ middle ++ "]"
  where
    middle = mconcat . intersperse "," $ build <$> arr
build (JsonObject obj) = "{" ++ middle ++ "}"
  where
    middle = mconcat . intersperse "," $ transform <$> M.toList obj
    transform (key, val) = "\"" ++ key ++ "\"" ++ ":" ++ build val

{-
  Function taking a type a and returns a list with a single element of type a.
-}
singleton :: a -> [a]
singleton a = [a]

{-
  Natural transformation of: Either a ~> Maybe
-}
rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _)  = Nothing
rightToMaybe (Right b) = Just b

{-
  Natural transformation of: Maybe ~> IO
-}
maybeToIO :: Maybe a -> IO a
maybeToIO Nothing  = fail "No value could be extracted."
maybeToIO (Just a) = return a

{-
  Given a sequence of (Token, String), returns a parser that can parse any one of the given Strings
  and overwrites the result with the given Token.
-}
tokenTransformSeq :: Monoid s => [(Token, String)] -> Parser Char s Token
tokenTransformSeq = asum . fmap transform
  where
    transform (token, str) = token <$ traverseP str

{-
  Given a sequence of (Token, Char), returns a parser that can parse any one of the given Chars
  and overwrites the result with the given Token.
-}
tokenTransform :: Monoid s => [(Token, Char)] -> Parser Char s Token
tokenTransform = asum . fmap transform
  where
    transform (token, char) = token <$ parseOne char

{-
  A lexer is a function taking a sequence of Char values, with an internal state of (),
  producing a result of type [Token].
-}
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

{-
  A parser is a function taking a sequence of Token values, with an internal state of (),
  producing a result of type Value.
-}
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
    commaSep parser' =
      (singleton <$> parser') <> many (parseOne Comma *> parser') <|> return []

{-
  Our frontend is a parser composed of the lexer sunk into the parser.
-}
frontend :: Parser Char () Value
frontend = lexer >$> parser

{-
  Function taking
    - the string to parse
  Returns
    - A Maybe Value -> a parsed json value (Maybe signals optionality, nullability)
-}
jsonFromString :: String -> Maybe Value
jsonFromString input = fmap snd . rightToMaybe $ runParser frontend input mempty

{-
  Function taking
    - the filepath to read from.
  Returns
    - an action that reads the content of the file and attempts to parse it to a value.
-}
jsonFromFile :: FilePath -> IO (Maybe Value)
jsonFromFile input = jsonFromString <$> readFile input

{-
  Function taking
    - a FilePath for the file to be written to
    - the JSON Value to serialize
  Returns
    - An action that writes to the given file the serialized json and returns no result.
-}
buildToFile :: FilePath -> Value -> IO ()
buildToFile file value = writeFile file $ build value

{-
  Entry point into the program -> an action returning no result.
-}
main :: IO ()
main = do
  let file = "input.json"
  value <- jsonFromFile file >>= maybeToIO
  pPrint value
  pPrint $ build value
  buildToFile file value

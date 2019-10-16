module Main where

import           Control.Applicative
import qualified HashMap             as HM
import           Text.Pretty.Simple  (pPrint)

import           Parser

-- What a token can be.
data Token
  = Identifier TokenIdent
  | LitInt TokenInt
  | LitStr TokenStr
  | ParenOpen
  | ParenClose
  | Semicolon
  | Colon
  | Comma
  | Add
  | Sub
  | Mul
  | Div
  | Equal
  | KwLet
  | KwIn
  | KwIf
  | KwThen
  | KwElse
  | KwRead
  | KwWrite
  | KwLoop
  | KwWhile
  | KwDo
  | KwIntT
  | KwStrT
  deriving (Eq, Show, Ord)

-- Various newtypes.
newtype TokenIdent = TokenIdent
  { unIdent :: String
  } deriving (Eq, Show, Ord)

newtype TokenInt = TokenInt
  { unInt :: Int
  } deriving (Eq, Show, Ord)

newtype TokenStr = TokenStr
  { unStr :: String
  } deriving (Eq, Show, Ord)

-- A token literal.
data Literal
  = LInt TokenInt
  | LStr TokenStr
  deriving (Show, Eq)

-- A symbol table is composed of a hashmap for identifiers
-- and one for literals.
data SymbolTable
  = SymbolTable (HM.HashMap TokenIdent) (HM.HashMap Literal)
  deriving (Show)

-- We need semigroup and monoid for symbol table because of the requiremenets for
-- traverseP and sequenceP.
instance Semigroup SymbolTable where
  (SymbolTable ident1 lit1) <> (SymbolTable ident2 lit2) =
    SymbolTable (ident1 `HM.union` ident2) (lit1 `HM.union` lit2)

instance Monoid SymbolTable where
  mempty = SymbolTable HM.empty HM.empty

-- Various helpers, extractors, symbol table update functions.
addIdent :: TokenIdent -> SymbolTable -> SymbolTable
addIdent token (SymbolTable idents lits) =
  SymbolTable (HM.insert token idents) lits

addLit :: Literal -> SymbolTable -> SymbolTable
addLit token (SymbolTable idents lits) =
  SymbolTable idents (HM.insert token lits)

toIdent :: Token -> Maybe String
toIdent (Identifier i) = Just (unIdent i)
toIdent _              = Nothing

toLitInt :: Token -> Maybe Int
toLitInt (LitInt i) = Just (unInt i)
toLitInt _          = Nothing

toLitStr :: Token -> Maybe String
toLitStr (LitStr s) = Just (unStr s)
toLitStr _          = Nothing

toIntT :: Token -> Maybe ()
toIntT KwIntT = Just ()
toIntT _      = Nothing

toStrT :: Token -> Maybe ()
toStrT KwStrT = Just ()
toStrT _      = Nothing

singleton :: a -> [a]
singleton x = [x]

-- Given a list of pairs (token, string), return a parser that
-- parser any one of that strings not followed by an alphanum_ character and
-- returns the associated token as parsing result.
tokenTransformSeq :: [(Token, String)] -> Parser Char SymbolTable Token
tokenTransformSeq = foldr (<|>) empty . fmap transform
  where
    transform (token, str) = token <$ traverseP str <* parseFail identCont
    identCont =
      parseRange '0' '9' <|> parseRange 'a' 'z' <|> parseRange 'A' 'Z' <|>
      parseOne '_'

-- Given a list of pairs (token, character), return a parser that
-- parser any one of that character and returns the associated token as parsing result.
tokenTransform :: [(Token, Char)] -> Parser Char SymbolTable Token
tokenTransform = foldr (<|>) empty . fmap transform
  where
    transform (token, char) = token <$ parseOne char

lexer :: Parser Char SymbolTable [Token]
lexer = mainParser <* wsParser <* parseEof
  where
    mainParser =
      many $
      wsParser *>
      (kwParser <|> identParser <|> intParser <|> strParser <|> symbolParser)
    wsParser = many $ parseOne ' ' <|> parseOne '\n'
    identParser = do
      let startParser =
            parseRange 'a' 'z' <|> parseRange 'A' 'Z' <|> parseOne '_'
          digitParser = parseRange '0' '9'
      start <- startParser
      body <- many $ startParser <|> digitParser
      if length body + 1 <= 250
        then do
          let token = TokenIdent $ start : body
          updateState (addIdent token) $ return $ Identifier token
        else empty
    intParser =
      let negParser = (singleton <$> parseOne '-') <> unsignedParser
          posParser = parseOne '+' *> unsignedParser
          unsignedParser = (singleton <$> parseRange '1' '9') <> many (parseRange '0' '9')
          zeroParser = singleton <$> parseOne '0'
          token = TokenInt . read <$> (zeroParser <|> negParser <|> posParser <|> unsignedParser)
      in do
        token' <- token
        updateState (addLit $ LInt token') $ return $ LitInt token'
    strParser = do
      token' <- TokenStr <$> (parseOne '"' *> parseWhileNe '"' <* parseOne '"')
      updateState (addLit $ LStr token') $ return $ LitStr token'
    symbolParser =
      tokenTransform
        [ (ParenOpen, '(')
        , (ParenClose, ')')
        , (Semicolon, ';')
        , (Colon, ':')
        , (Equal, '=')
        , (Comma, ',')
        , (Add, '+')
        , (Sub, '-')
        , (Mul, '*')
        , (Div, '/')
        ]
    kwParser =
      tokenTransformSeq
        [ (KwLet, "let")
        , (KwIn, "in")
        , (KwIf, "if")
        , (KwThen, "then")
        , (KwElse, "else")
        , (KwRead, "read")
        , (KwWrite, "write")
        , (KwLoop, "loop")
        , (KwWhile, "while")
        , (KwDo, "do")
        , (KwIntT, "Int")
        , (KwStrT, "String")
        ]

-- A minix program is a list of minix propositions.
data MinixProgram
  = MinixProgram [MinixProp]
  deriving (Show)

-- A minix proposition is a let | if | read | write | loop | do statement.
data MinixProp
  = LetStmt [MinixDecl] [MinixProp]
  | IfStmt MinixExpr MinixProp MinixProp
  | ReadStmt MinixIdentifier
  | WriteStmt MinixExpr
  | LoopStmt MinixProp MinixExpr
  | DoStmt [MinixProp]
  deriving (Show)

-- A minix literal value can be an integer, string, identifier, a tuple2,
-- or some operation of the above.
data MinixExpr
  = MinixInt Int
  | MinixStr String
  | MinixIdent MinixIdentifier
  | MinixCompound MinixExpr MinixExpr
  | MinixAdd MinixExpr MinixExpr
  | MinixSub MinixExpr MinixExpr
  | MinixMul MinixExpr MinixExpr
  | MinixDiv MinixExpr MinixExpr
  | MinixTerm MinixExpr
  deriving (Show)

-- Newtyped identifier.
data MinixIdentifier
  = MinixIdentifier String
  deriving (Show)

-- A minix type is an int, string, or tuple2 type.
data MinixType
  = MinixIntT
  | MinixStrT
  | MinixCompoundT MinixType MinixType
  deriving (Show)

-- A minix declaration contains an identifier, its type and initialization value.
data MinixDecl
  = MinixDecl MinixIdentifier MinixType MinixExpr
  deriving (Show)

parser :: Parser Token SymbolTable MinixProgram
parser = program <* parseEof
  where
    program = MinixProgram <$> many stmt
    stmt = letStmt <|> ifStmt <|> readStmt <|> writeStmt <|> loopStmt <|> doStmt
    letStmt =
      LetStmt <$> (parseOne KwLet *> many declExpr) <*>
      (parseOne KwIn *> many stmt)
    ifStmt =
      IfStmt <$> (parseOne KwIf *> minixExpr) <*> (parseOne KwThen *> stmt) <*>
      (parseOne KwElse *> stmt)
    readStmt = ReadStmt <$> (parseOne KwRead *> minixIdent)
    writeStmt = WriteStmt <$> (parseOne KwWrite *> minixExpr)
    loopStmt =
      LoopStmt <$> (parseOne KwLoop *> stmt) <*> (parseOne KwWhile *> minixExpr)
    doStmt = DoStmt <$> (parseOne KwDo *> some stmt <* parseOne Semicolon)
    declExpr =
      MinixDecl <$> (minixIdent <* parseOne Colon) <*>
      (minixType <* parseOne Equal) <*>
      minixExpr
    minixExpr = minixAdd <|> minixSub <|> minixTerm
    minixTerm = minixMul <|> minixDiv <|> MinixTerm <$> minixFactor
    minixFactor = parseOne ParenOpen *> minixExpr <* parseOne ParenClose
                  <|> MinixIdent <$> minixIdent
                  <|> MinixStr <$> parseOptional toLitStr
                  <|> MinixInt <$> parseOptional toLitInt
                  <|> MinixCompound <$> (parseOne ParenOpen *> minixExpr <* parseOne Comma) <*> (minixExpr <* parseOne ParenClose)
    minixAdd = MinixAdd <$> (minixTerm <* parseOne Add) <*> minixExpr
    minixSub = MinixSub <$> (minixTerm <* parseOne Sub) <*> minixExpr
    minixMul = MinixMul <$> (minixFactor <* parseOne Mul) <*> minixExpr
    minixDiv = MinixDiv <$> (minixFactor <* parseOne Div) <*> minixExpr
    minixIdent = MinixIdentifier <$> parseOptional toIdent
    minixType =
      MinixIntT <$ parseOptional toIntT <|> MinixStrT <$ parseOptional toStrT <|>
      minixCompoundType
    minixCompoundType =
      MinixCompoundT <$> (parseOne ParenOpen *> minixType <* parseOne Comma) <*>
      (minixType <* parseOne ParenClose)

-- A parser frontend is a lexer parser sunk into our parser.
frontend :: Parser Char SymbolTable MinixProgram
frontend = lexer >$> parser

-- Read the program, if parsing is ok, print the final state and AST, otherwise print the error.
main :: IO ()
main = do
  programText <- readFile "input.minix"
  case runParser frontend programText mempty of
    Left err           -> pPrint err
    Right (s, program) -> do
      putStrLn ""
      pPrint s
      putStrLn ""
      pPrint program

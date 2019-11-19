{-# LANGUAGE FlexibleContexts #-}

module Parser
  ( ParseError
  , Parser
  , runParser
  , (>$>)
  , updateState
  , parseRange
  , parseOne
  , parseFail
  , parseWhileEq
  , parseWhileNe
  , parseOptional
  , parseEof
  , traverseP
  , sequenceP
  ) where

import           Control.Applicative
import qualified Data.List.NonEmpty  as NE

-- Type synonim for our result, either a parse error or the final state and
-- the result that was computed.
type Result i s r = Either ParseError (s, r)

-- A parse error is either a flat error string or a
-- node that has sub-errors; useful for displaying parsers that were tried and failed,
-- etc..
data ParseError
  = ErrorOne String
  | ErrorMore String [ParseError]
  deriving (Show)

-- A Parse result is either a success or a failure.
-- Both cases contain the inputs that remained unconsumed and their current state,
-- also the success contains the success value, and the failure contains the
-- parse error.
data ParseResult i s r
  = Success [i] s r
  | Failure [i] s ParseError
  deriving (Show)

-- Instance functor to be able to instantiate applicative.
instance Functor (ParseResult i s) where
  fmap f (Success i s r)   = Success i s (f r)
  fmap _ (Failure i s err) = Failure i s err

-- Instance monoid to be able to instantiate applicative.
instance Monoid s => Applicative (ParseResult i s) where
  pure = Success [] mempty
  (Failure i s err) <*> _ = Failure i s err
  _ <*> (Failure i s err) = Failure i s err
  (Success i s r) <*> (Success i' s' r') = Success i' (s <> s') (r r')

-- A parser: a function consuming a stream of elements of type i, with an
-- initial state s, producing a parse result `i s r`.
newtype Parser i s r =
  Parser
    { unParser :: [i] -> s -> ParseResult i s r
    }

-- Low precedence operator - can be read as 'sinkInto'
-- If a parser produces another stream, that one can be fed into another parser
-- to produce a third parser.
infixr 2 >$>

(>$>) :: Parser i s [r] -> Parser r s r' -> Parser i s r'
source >$> sink =
  Parser $ \i s ->
    case unParser source i s of
      Success is ss rs ->
        case unParser sink rs ss of
          Success _ s' r'  -> Success is s' r'
          Failure _ s' err -> Failure is s' err
      Failure is ss err -> Failure is ss err

-- Given an input stream and an initial state, run our parser.
runParser :: Parser i s r -> [i] -> s -> Result i s r
runParser parser input initialState =
  extractResult $ unParser parser input initialState
  where
    extractResult (Success _ s r) = Right (s, r)
    extractResult (Failure _ _ r) = Left r

-- Return a parser that parser just one element, i.
parseOne :: (Eq i, Show i, Ord i) => i -> Parser i s i
parseOne input = parseRange input input

-- For an update function and a parser, return a parser for which the
-- update function was applied.
updateState :: (s -> s) -> Parser i s r -> Parser i s r
updateState f parser =
  Parser $ \i s ->
    case unParser parser i s of
      Success i' s' r'   -> Success i' (f s') r'
      failure@Failure {} -> failure

-- A function similar to traverse, however, Traversable cannot be instantiated
-- for our parser because we cannot instantiate foldable.
traverseP :: (Monoid s, Show i, Ord i) => [i] -> Parser i s [i]
traverseP = sequenceP . fmap parseOne

-- A function similar to sequence, however, Traversable cannot be instantiated
-- for our parser because we cannot instantiate foldable.
sequenceP :: (Monoid s) => [Parser i s r] -> Parser i s [r]
sequenceP parsers = Parser $ \i s -> fromResult i s $ runParsers parsers i s
  where
    runParsers parsers' i s =
      case parsers' of
        [] -> Success i s []
        (p:ps) ->
          case unParser p i s of
            Success i' s' r -> Success i' s' (r :) <*> runParsers ps i' s'
            Failure i' s' errMsg -> Failure i' s' errMsg
    fromResult i s success@Success {} = success
    fromResult i s (Failure _ _ err)  = Failure i s err

-- Given a parser, return another parser that succeeds when this one fails
-- and consumes no input.
parseFail :: Parser i s r -> Parser i s ()
parseFail parser =
  Parser $ \i s ->
    case unParser parser i s of
      Success i' s' result' ->
        Failure i s $ ErrorOne "Failing parser succeeded instead."
      Failure i s _ -> Success i s ()

-- Given a function returning Maybe, create a parser that succeeds only when
-- consuming one input yields a `Just _` value.
parseOptional :: (Show i) => (i -> Maybe r) -> Parser i s r
parseOptional f =
  Parser $ \i s ->
    case i of
      [] ->
        Failure [] s $
        ErrorOne $ "Attempted to parse '" <> show i <> "' but EOF found."
      (x:xs) ->
        case f x of
          Nothing ->
            Failure (x : xs) s $
            ErrorOne $
            "Attempting to parse'" <>
            show x <> "', but predicate wasn't satisfied."
          Just r -> Success xs s r

-- Similar to parseOne, but using ranges.
parseRange :: (Eq i, Show i, Ord i) => i -> i -> Parser i s i
parseRange lower upper =
  Parser $ \i s ->
    case i of
      [] ->
        Failure [] s $
        ErrorOne $ "Attempted to parse '" <> show i <> "' but EOF found."
      (x:xs) ->
        if x >= lower && x <= upper
          then Success xs s x
          else Failure (x : xs) s $
               ErrorOne $
               "Attempted to parse [" <>
               show lower <>
               ", " <>
               show upper <> "] but could not match with '" <> show x <> "'."

-- Parse while not equal - create a parser that consumes input until an element
-- is equal to the given element.
parseWhileNe :: (Show i, Eq i) => i -> Parser i s [i]
parseWhileNe input =
  Parser $ \i s ->
    let result@(i', r) = span (/= input) i
     in Success r s i'

-- Parse while equal - create a parser that consumes input until an element
-- is equal to the given element.
parseWhileEq :: (Eq i) => i -> Parser i s [i]
parseWhileEq input =
  Parser $ \i s ->
    let (i', r) = span (== input) i
     in Success r s i'

-- Create a parser that succeeds only when there is no input left to consume.
parseEof :: Show i => Parser i s ()
parseEof =
  Parser $ \i s ->
    case i of
      [] -> Success i s ()
      (x:xs) ->
        Failure i s $
        ErrorOne $
        "Attempted to parse EOF - elements still exist: " <> show i <> "."

-- Given two parsers, return a third one that tries the first parser,
-- and if it does not succeed, tries the second one.
-- If either succeeds, the result of that one is returned.
-- Evaluation is left to right.
parseOr :: Parser i s r -> Parser i s r -> Parser i s r
parseOr parser1 parser2 =
  Parser $ \i s ->
    case unParser parser1 i s of
      success@Success {} -> success
      Failure _ _ errMsg ->
        case unParser parser2 i s of
          success'@Success {} -> success'
          Failure _ _ errMsg' ->
            Failure i s $
            ErrorMore "Could not match alternatives." [errMsg, errMsg']

-- Given a map function and a parser, return a parser that runs the given
-- parser and transforms the result using the given function.
parseMap :: (r1 -> r2) -> Parser i s r1 -> Parser i s r2
parseMap f parser =
  Parser $ \i s ->
    case unParser parser i s of
      Success i' s' r   -> Success i' s' (f r)
      Failure i' s' err -> Failure i' s' err

-- parseApplicative - applicative combination of parsers.
parseAp :: Parser i s (r1 -> r2) -> Parser i s r1 -> Parser i s r2
parseAp parserF parser =
  Parser $ \i s ->
    case unParser parserF i s of
      Success i' s' f ->
        case unParser parser i' s' of
          Success i'' s'' r'      -> Success i'' s'' (f r')
          Failure input state err -> Failure input state err
      Failure input state err -> Failure input state err

-- parseBind - monadic composition of parsers.
parseBind :: Parser i s r -> (r -> Parser i s r') -> Parser i s r'
parseBind parser f =
  Parser $ \i s ->
    case unParser parser i s of
      Success input state result -> unParser (f result) input state
      Failure input state result -> Failure input state result

-- Given a value, create a parser that consumes no input and returns the given
-- value.
parsePure :: r -> Parser i s r
parsePure result = Parser $ \i s -> Success i s result

-- Return a parser that consumes nothing and always fails.
parseEmpty :: Parser i s r
parseEmpty = Parser $ \i s -> Failure i s $ ErrorOne "Empty parser."

-- Various type-class instances to make this library usable.
instance Functor (Parser i s) where
  fmap = parseMap

instance Applicative (Parser i s) where
  pure = parsePure
  (<*>) = parseAp

instance Monad (Parser i s) where
  (>>=) = parseBind

instance Alternative (Parser i s) where
  empty = parseEmpty
  (<|>) = parseOr

instance Semigroup r => Semigroup (Parser i s r) where
  lhs <> rhs = do
    lhs' <- lhs
    rhs' <- rhs
    return $ lhs' <> rhs'

instance Monoid r => Monoid (Parser i s r) where
  mempty = parsePure mempty

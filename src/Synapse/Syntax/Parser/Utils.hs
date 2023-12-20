{-# LANGUAGE LambdaCase #-}

module Synapse.Syntax.Parser.Utils
  where

import Text.Megaparsec

import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Char

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set

import Control.Applicative hiding (some, many)
import Control.Monad

import Data.Void
import Data.Functor

type Parser = Parsec Void String

parse' :: Parser a -> String -> a
parse' = parse'' "<input>"

parse'' :: String -> Parser a -> String -> a
parse'' sourceName p str =
  case parse p sourceName str of
    Left err -> error $ errorBundlePretty err
    Right x -> x

sc :: Parser ()
sc = L.space
  (takeWhile1P (Just "space") (`elem` " \t") $> ()) --space1
  mempty --(L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

scNewline :: Parser ()
scNewline = L.space
  space1
  mempty --(L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

lexemeNewline :: Parser a -> Parser a
lexemeNewline = L.lexeme scNewline

symbolNewline :: String -> Parser String
symbolNewline = L.symbol scNewline

keywordNewline :: String -> Parser String
keywordNewline str = lexemeNewline (string str <* notFollowedBy alphaNumChar)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

keyword :: String -> Parser String
keyword str = lexeme (string str <* notFollowedBy alphaNumChar)

parseInt :: Parser Int
parseInt =
    read <$>
    liftA2 (++) parseSign (some digitChar)
  where
    parseSign :: Parser String
    parseSign =
      optional (string "-") >>= \case
        Nothing -> pure []
        Just xs -> pure xs


parseIdentifier :: Parser String
parseIdentifier = label "identifier" $ do
    ident <- liftA2 (:) parseFirst (many parseTailChar)
    guard (not (isInt ident))
    pure ident
  where
    parseFirst, parseTailChar :: Parser Char
    parseFirst = letterChar <|> oneOf "!@#$%^&*+-.:~=<>/|_"
    parseTailChar = parseFirst <|> digitChar

isInt :: String -> Bool
isInt ('-':xs) = isNatural xs
isInt xs       = isNatural xs

isNatural :: String -> Bool
isNatural = all isDigit

parserFailure :: Maybe String -> String -> Parser a
parserFailure unexpected expected =
  failure (fmap (Label . NonEmpty.fromList) unexpected) (Set.singleton (Label (NonEmpty.fromList expected)))

parserGuard :: Bool -> Maybe String -> String -> Parser ()
parserGuard True _ _ = pure ()
parserGuard False unexpected expected = parserFailure unexpected expected


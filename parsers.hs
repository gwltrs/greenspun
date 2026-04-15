module Parsers where

import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Tuple
import Utils
import Data.List (intercalate)
import Data.Maybe (fromJust)

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
    fmap f (Parser p) =
        Parser $ \input -> do
            (input', x) <- p input
            Just (input', f x)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) =
        Parser $ \input -> do
            (input', f) <- p1 input
            (input'', a) <- p2 input'
            Just (input'', f a)

instance Monad Parser where
    return = pure
    (Parser p) >>= f =
        Parser $ \input -> do
            (input', x) <- p input
            runParser (f x) input'

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \i -> p1 i <|> p2 i

instance MonadPlus Parser where

char :: Char -> Parser Char
char c = Parser { runParser = f }
    where
        f (x:xs) = if x == c then Just (xs, c) else Nothing
        f [] = Nothing

notEmpty :: Parser [a] -> Parser [a]
notEmpty = mfilter (not . null)

string :: String -> Parser String
string = traverse char

manySepBy :: Parser a -> Parser b -> Parser [b]
manySepBy s p = liftA2 (:) p (many (s *> p)) <|> pure []

symbol :: Parser String
symbol = whileNE (\c -> isVisible c && c /= '(' && c /= ')')
 
while :: (Char -> Bool) -> Parser String
while f = Parser $ Just . swap . span f

whileNE :: (Char -> Bool) -> Parser String
whileNE = notEmpty . while

ws :: Parser String
ws = while isWhitespace

wsNE :: Parser String
wsNE = notEmpty ws

data Sexp = List [Sexp] | Atom String deriving Eq

instance Show Sexp where
    show (Atom a) = a
    show (List l) = "(" <> (intercalate " " (show <$> l)) <> ")"

listParser :: Parser Sexp
listParser = char '(' *> (List <$> sexpsParser) <* char ')'

atomParser :: Parser Sexp
atomParser = fmap Atom symbol

sexpParser :: Parser Sexp
sexpParser = atomParser <|> listParser

sexpsParser :: Parser [Sexp]
sexpsParser = ws *> manySepBy ws sexpParser <* ws

unsafeSexp :: String -> Sexp
unsafeSexp s = snd $ fromJust $ runParser sexpParser s
module Parsers.String where

import Type.Parser.String
import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Tuple
import Utils
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Type.Sexp

char :: Char -> StringParser Char
char c = Parser { runParser = f }
    where
        f (x:xs) = if x == c then Just (xs, c) else Nothing
        f [] = Nothing

notEmpty :: StringParser [a] -> StringParser [a]
notEmpty = mfilter (not . null)

string :: String -> StringParser String
string = traverse char

manySepBy :: StringParser a -> StringParser b -> StringParser [b]
manySepBy s p = liftA2 (:) p (many (s *> p)) <|> pure []

symbol :: StringParser String
symbol = whileNE (\c -> isVisible c && c /= '(' && c /= ')')
 
while :: (Char -> Bool) -> StringParser String
while f = Parser $ Just . swap . span f

whileNE :: (Char -> Bool) -> StringParser String
whileNE = notEmpty . while

ws :: StringParser String
ws = while isWhitespace

wsNE :: StringParser String
wsNE = notEmpty ws

list :: StringParser Sexp
list = char '(' *> (List <$> sexps) <* char ')'

atom :: StringParser Sexp
atom = fmap Atom symbol

sexp :: StringParser Sexp
sexp = atom <|> list

unsafeSexp :: String -> Sexp
unsafeSexp s = snd $ fromJust $ runParser sexp s

sexps :: StringParser [Sexp]
sexps = ws *> manySepBy ws sexp <* ws
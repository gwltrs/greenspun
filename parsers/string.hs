module Parsers.String where

import Type.Parser.String
import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Tuple
import Utils
import Data.List (intercalate)
import Data.Maybe (fromJust, catMaybes)
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
symbol = strLit <|> nonWS
    where
        strLit = (\s -> "\"" ++ s ++ "\"") <$> (char '"' *> whileNE (/= '"') <* char '"')
        nonWS = whileNE (\c -> isVisible c && c /= '(' && c /= ')' && c /= ';')
 
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
sexp = atom <|> list --(Nothing <$ comment) <|> (Just <$> )
    
    -- Just <$> (atom <|> list)

-- unsafeSexp :: String -> Sexp
-- unsafeSexp s = snd $ fromJust $ runParser sexp s

sexps :: StringParser [Sexp]
sexps = ws *> (catMaybes <$> manySepBy ws sexpMaybe) <* ws
    where
        sexpMaybe :: StringParser (Maybe Sexp)
        sexpMaybe = (Nothing <$ comment) <|> (Just <$> sexp)

unsafe :: StringParser a -> (String -> a)
unsafe (Parser { runParser = f }) s = snd $ fromJust $ f s

comment :: StringParser ()
comment = char ';' *> (lineComment <|> void sexp)
    where
        lineComment = (void wsNE <|> void (char ';')) *> void (while (/= '\n'))


-- lineComment :: StringParser ()
-- lineComment = char ';' *> (void wsNE <|> void (char ';')) *> void (while (/= '\n'))

-- sexpComment :: StringParser ()
-- sexpComment = char ';' *> void sexp
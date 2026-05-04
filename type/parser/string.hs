{-# LANGUAGE LambdaCase #-}

module Type.Parser.String where

import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Tuple
import Utils
import Data.List (intercalate)
import Data.Maybe (fromJust)

newtype StringParser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor StringParser where
    fmap f (Parser p) =
        Parser $ \input -> do
            (input', x) <- p input
            Just (input', f x)

instance Applicative StringParser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) =
        Parser $ \input -> do
            (input', f) <- p1 input
            (input'', a) <- p2 input'
            Just (input'', f a)

instance Monad StringParser where
    return = pure
    (>>=) :: StringParser a -> (a -> StringParser b) -> StringParser b
    (Parser p) >>= f =
        Parser $ \input -> do
            (input', x) <- p input
            runParser (f x) input'

instance Alternative StringParser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \i -> p1 i <|> p2 i

instance MonadPlus StringParser where
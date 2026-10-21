{-# LANGUAGE LambdaCase #-}

-- | A list monad transformer that satisfies the monad laws.
-- Depends only on base. Vendor this file as-is.
module ListT
  ( ListT(..)
  , Step(..)
  , lift
  , fromList
  , toList
  , foldListT
  , take
  , cons
  ) where

import Prelude hiding (take)
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..), ap, liftM)
import Control.Monad.IO.Class (MonadIO(..))

-- | A monadic step: either the end of the list, or an element
-- followed by the (effectful) rest.
data Step m a = Nil | Cons a (ListT m a)

newtype ListT m a = ListT { unListT :: m (Step m a) }

------------------------------------------------------------
-- Instances
------------------------------------------------------------

instance Monad m => Functor (ListT m) where
  fmap f (ListT m) = ListT $ m >>= \case
    Nil       -> return Nil
    Cons x xs -> return (Cons (f x) (fmap f xs))

instance Monad m => Applicative (ListT m) where
  pure x = ListT (return (Cons x (ListT (return Nil))))
  (<*>)  = ap

instance Monad m => Monad (ListT m) where
  ListT m >>= f = ListT $ m >>= \case
    Nil       -> return Nil
    Cons x xs -> unListT (f x <|> (xs >>= f))

instance Monad m => Alternative (ListT m) where
  empty = ListT (return Nil)
  ListT m <|> ys = ListT $ m >>= \case
    Nil       -> unListT ys
    Cons x xs -> return (Cons x (xs <|> ys))

instance Monad m => MonadPlus (ListT m)

instance Monad m => MonadFail (ListT m) where
  fail _ = empty

instance Monad m => Semigroup (ListT m a) where
  (<>) = (<|>)

instance Monad m => Monoid (ListT m a) where
  mempty = empty

instance MonadIO m => MonadIO (ListT m) where
  liftIO = lift . liftIO

------------------------------------------------------------
-- Operations
------------------------------------------------------------

-- | Lift a monadic action into a one-element list.
lift :: Monad m => m a -> ListT m a
lift m = ListT $ m >>= \x -> return (Cons x empty)

-- | Prepend an element.
cons :: Monad m => a -> ListT m a -> ListT m a
cons x xs = ListT (return (Cons x xs))

fromList :: Monad m => [a] -> ListT m a
fromList = foldr cons empty

-- | Run all effects and collect the results.
toList :: Monad m => ListT m a -> m [a]
toList (ListT m) = m >>= \case
  Nil       -> return []
  Cons x xs -> (x :) `liftM` toList xs

-- | Monadic left fold, consuming the list incrementally.
foldListT :: Monad m => (b -> a -> m b) -> b -> ListT m a -> m b
foldListT f z (ListT m) = m >>= \case
  Nil       -> return z
  Cons x xs -> f z x >>= \z' -> foldListT f z' xs

-- | Take at most n elements (stops running effects after n).
take :: Monad m => Int -> ListT m a -> ListT m a
take n (ListT m)
  | n <= 0    = empty
  | otherwise = ListT $ m >>= \case
      Nil       -> return Nil
      Cons x xs -> return (Cons x (take (n - 1) xs))
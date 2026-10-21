-- | A MaybeT monad transformer. Depends only on base. Vendor as-is.
module MaybeT
  ( MaybeT(..)
  , lift
  , hoistMaybe
  , mapMaybeT
  ) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class (MonadIO(..))
import Data.Traversable (Traversable(traverse))

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

------------------------------------------------------------
-- Instances
------------------------------------------------------------

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT m) = MaybeT (fmap (fmap f) m)

instance Monad m => Applicative (MaybeT m) where
  pure x = MaybeT (return (Just x))
  MaybeT mf <*> MaybeT mx = MaybeT $
    mf >>= maybe (return Nothing) (\f ->
      mx >>= maybe (return Nothing) (\x ->
        return (Just (f x))))

instance Monad m => Monad (MaybeT m) where
  MaybeT m >>= f = MaybeT $
    m >>= maybe (return Nothing) (runMaybeT . f)

instance Monad m => Alternative (MaybeT m) where
  empty = MaybeT (return Nothing)
  MaybeT a <|> MaybeT b = MaybeT $
    a >>= maybe b (return . Just)

instance Monad m => MonadPlus (MaybeT m)

instance Monad m => Fail.MonadFail (MaybeT m) where
  fail _ = MaybeT (return Nothing)

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance Foldable f => Foldable (MaybeT f) where
  foldMap f (MaybeT m) = foldMap (foldMap f) m

instance (Traversable f) => Traversable (MaybeT f) where
    traverse f (MaybeT a) = MaybeT <$> traverse (traverse f) a
------------------------------------------------------------
-- Operations
------------------------------------------------------------

-- | Lift an action from the underlying monad (always succeeds).
lift :: Monad m => m a -> MaybeT m a
lift m = MaybeT (fmap Just m)

-- | Embed a pure 'Maybe'.
hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

-- | Transform the underlying computation.
mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Adj.Utils where

import Adj

import "base" Text.Show (Show (show))

import "ghc-prim" GHC.Prim (State#, RealWorld)
import "ghc-prim" GHC.Types (IO (IO))

deriving instance (Show l, Show r) => Show (l :*: r)
deriving instance (Show l, Show r) => Show (l :+: r)
deriving instance (Show l, Show r) => Show (Flat (:*:) l r)
deriving instance (Show l, Show r) => Show (Flat (:+:) l r)
deriving instance (Show l, Show r) => Show (Dual (:*:) r l)
deriving instance (Show l, Show r) => Show (Dual (:+:) r l)
deriving instance Show o => Show (Identity o)
deriving instance Show Void
deriving instance Show Unit

instance Show o => Show (Structural o) where
	show (Structural x) = show x

deriving via (Structural (f (g o))) instance
	Show (f (g o)) => Show ((=!?=) f g o)

deriving via (Structural (f (g (f' o)))) instance
	Show (f (g (f' o))) => Show ((=!?!=) f g f' o)

deriving via (Structural (f (g o) (h o))) instance
	Show (f (g o) (h o)) => Show ((=!!??=) f g h o)

deriving via (Structural ((=!!??=) p Identity (f =!?= Generation p f) o))
	instance Show (p (Identity o) ((=!?=) f (Generation p f) o)) => Show (Generation p f o)

instance Functor (-->) (-->) IO where
	map (Flat m) = Flat .: \x -> bindIO x (returnIO . m)

instance Functor ((-/->) IO) (-->) IO where
	map (Kleisli (Flat m)) = Flat .: \x -> bindIO x m

instance Component (-->) (Day (-->) IO IO (:*:) (:*:)) IO where
	component = Flat .: \case
		Day (l :*: r) (Flat m) -> bindIO l .: \l' -> bindIO r .: \r' -> returnIO .: m (l' :*: r')

instance Component (-->) (Day (-->) Identity IO (:*:) (:*:)) IO where
	component = Flat .: \case
		Day (Identity l :*: r) (Flat m) -> bindIO r .: \r' -> returnIO .: m (l :*: r')

instance Component (-->) (Day (-->) IO Identity (:*:) (:*:)) IO where
	component = Flat .: \case
		Day (l :*: Identity r) (Flat m) -> bindIO l .: \l' -> returnIO .: m (l' :*: r)

instance Component (-->) ((-->) Unit) IO where
	component = Flat .: \case
		Flat m -> returnIO .: m Unit

instance Component (-->) Identity IO where
	component = Flat .: \case
		Identity x -> returnIO x

returnIO :: a -> IO a
returnIO x = IO (\ s -> (# s, x #))

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO m) k = IO (\s -> case m s of (# new_s, a #) -> unIO (k a) new_s)

thenIO :: IO a -> IO b -> IO b
thenIO (IO m) k = IO (\s -> case m s of (# new_s, _ #) -> unIO k new_s)

unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO a) = a

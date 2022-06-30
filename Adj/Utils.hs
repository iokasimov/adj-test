{-# LANGUAGE UndecidableInstances #-}

module Adj.Utils where

import Adj

import "base" Text.Show (Show (show))

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

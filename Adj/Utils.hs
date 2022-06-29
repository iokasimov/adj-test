module Adj.Utils where

import Adj

import "base" Text.Show (Show)

deriving instance (Show left, Show right) => Show (left :*: right)
deriving instance (Show left, Show right) => Show (left :+: right)
deriving instance Show Void
deriving instance Show Unit

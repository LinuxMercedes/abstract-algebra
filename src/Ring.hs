module Ring (
    Ring(..),
    URing(..),
    Field(..),
  ) where

import Group

class Group a => Ring a where
  (.*.) :: a -> a -> a

class Ring a => URing a where
  identity :: a

class URing a => Field a where
  inverse :: a -> a
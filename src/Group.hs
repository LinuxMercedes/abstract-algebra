module Group (
    Group(..), FiniteGroup(..),
  ) where

class Eq a => Group a where
  (.+.) :: a -> a -> a
  (.-.) :: a -> a -> a
  a .-. b = a .+. (inverse b)

  identity :: a
  inverse :: a -> a

class Group a => FiniteGroup a where
  groupOrder :: a -> Integer
  elements :: [a]

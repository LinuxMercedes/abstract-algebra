{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IntegersModN (
    Z7(..), Z8(..), Z9(..),
    extendedEuclidean
  ) where

import Group as G
import Ring as R

newtype Z7 = Z7 { unZ7 :: Int } deriving (Eq, Show, Read, Num, Integral, Enum, Real, Ord)

instance Group Z7 where
  a .+. b = (a + b) `mod` 7
  identity = 0
  inverse a = (7 - a) `mod` 7

instance FiniteGroup Z7 where
  groupOrder = const 7
  elements = [0..6]

instance Ring Z7 where
  a .*. b = (a * b) `mod` (fromIntegral $ groupOrder a)

instance URing Z7 where
  identity = 1

instance Field Z7 where
  inverse a = let order = fromIntegral $ groupOrder a
              in  (snd $ extendedEuclidean order a) `mod` order



newtype Z8 = Z8 { unZ8 :: Int } deriving (Eq, Show, Read, Num, Integral, Enum, Real, Ord)

instance Group Z8 where
  a .+. b = (a + b) `mod` 8
  identity = 0
  inverse a = (8 - a) `mod` 8

instance FiniteGroup Z8 where
  groupOrder = const 8
  elements = [0..7]

instance Ring Z8 where
  a .*. b = (a * b) `mod` (fromIntegral $ groupOrder a)

instance URing Z8 where
  identity = 1



newtype Z9 = Z9 { unZ9 :: Int } deriving (Eq, Show, Read, Num, Integral, Enum, Real, Ord)

instance Group Z9 where
  a .+. b = (a + b) `mod` 9
  identity = 0
  inverse a = (9 - a) `mod` 9

instance FiniteGroup Z9 where
  groupOrder = const 9
  elements = [0..8]

instance Ring Z9 where
  a .*. b = (a * b) `mod` (fromIntegral $ groupOrder a)

instance URing Z9 where
  identity = 1


extendedEuclidean a b =
      case divMod a b of
           (q, 1) -> (1, -q)
           (quot, rem) -> let (s,t) = extendedEuclidean b rem
                          in  (t, s - (t * quot))

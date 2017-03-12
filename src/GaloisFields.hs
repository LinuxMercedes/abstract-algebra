{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GaloisFields (
    GaloisField(..),
    GF2_3(..),
  ) where

import Group as G
import Ring as R

import Data.Bits
import Data.Word (Word8)
import Prelude hiding (quotRem)

class (Group a, Num a, Bits a) => GaloisField a where
  irredPoly :: a -> a
  irredPolyDegree :: a -> Integer
  {-| Degree of a polynomial

  >>> p = read "101" :: AESField
  >>> degree p
  2
  -}
  degree :: a -> Int
  degree 0 = 0
  degree 1 = 0
  degree p = 1 + (degree $ shiftR p 1)


newtype GF2_3 = GF2_3 { unGF2_3 :: Word8 } deriving (Eq, Show, Read, Num, Integral, Enum, Real, Ord, Bits)

instance Group GF2_3 where
  a .+. b = a `xor` b
  identity = 0
  inverse a = a

instance FiniteGroup GF2_3 where
  groupOrder = const 8
  elements = [0..7]

instance Ring GF2_3 where
  x .*. y = multField x y

instance URing GF2_3 where
  identity = 1

instance GaloisField GF2_3 where
  irredPoly = const 3
  irredPolyDegree = const 4


-- | Finite field (GF(2)[x]/<m>) bit-representation multiplication
--multField :: (FiniteField a) => a -> a -> a
multField 0 b = 0
multField 1 b = b
multField a b = multField' a b 0

{-|
  Efficient finite field multiplication
  Since finite field multiplication is based on repeatedly multiplying by x,
  maintain an accumulator that collects the product as a sum of successive values of b
  (in standard multiplication, we'd shift a right and b left by one each step;
  here, we shift a right and multiply b by x, which is the same thing in a finite field)
-}
--multField' :: (FiniteField a) => a -> a -> a -> a
multField' 0 b acc = acc
multField' a b acc
  | testBit a 0 = multField' (shift a (-1)) (multX b) (acc .+. b)
  | otherwise   = multField' (shift a (-1)) (multX b) acc

-- | Multiply a bit representation of a polynomial by x in a finite field
--multX :: (FiniteField a) => a -> a
multX a
  | testBit a (fs - 2) = (shift a 1) `clearAboveBit` (fs - 1) .+. (irredPoly a)
  | otherwise            = shift a 1
  where
    fs = fromIntegral $ irredPolyDegree a

-- HACK: Natural doesn't implement complement, and thus clearBit =[
-- https://hackage.haskell.org/package/base-4.9.0.0/docs/src/GHC.Natural.html#line-307
--clearAboveBit :: (FiniteField a) => a -> Int -> a
clearAboveBit bits pos = bits .&. ((shift 1 pos) - 1)


instance Field GF2_3 where
  inverse 0 = 0
  inverse p = -- This is pretty hacky to deal with how we're storing irred now
    let fs = (fromIntegral $ irredPolyDegree p) - 1 -- unroll one layer of extendedEuclidean since we don't have a complete irred to hand off
        shiftamt = fs - (degree p) -- shift p over to do long division
        irred' = (irredPoly p) .-. ((shift p shiftamt) `clearAboveBit` fs)
        (quot',rem) = quotRem irred' p -- Finish doing the division
        quot = quot' .|. (shift 1 shiftamt) -- just add the one thing we took off
        (s,t) = extendedEuclidean p rem -- Thankfully this is lazily evaluated
    in  case (quot, rem) of
             (quot, 1)   -> quot
             (quot, rem) -> s .-. (t .*. quot)

{-| Extended euclidean GCD algorithm; used by 'inverse'
Note: currently assumes GCD = 1
TODO: check degrees
-}
--extendedEuclidean :: (FiniteField a) => a -> a -> (a,a)
extendedEuclidean a b =
  case quotRem a b of
    (quot, 1)   -> (1, quot)
    (quot, rem) -> let (s,t) = extendedEuclidean b rem
                    in (t, s .-. (t .*. quot))

{-| Polynomial long division outside of a field
Used by extendedEuclidean.
-}
--quotRem :: (FiniteField a) => a -> a -> (a,a)
quotRem p 0 = (0, p)
quotRem dividend divisor
  | (degree divisor) > (degree dividend)  = (0, dividend)
  | (degree divisor) == (degree dividend) = (1, dividend .-. divisor)
  | otherwise =
    let shiftamt = (degree dividend) - (degree divisor)
        diff = dividend .-. (shift divisor shiftamt)
        (quot, rem) = quotRem diff divisor
    in (((shift 1 shiftamt) .|. quot), rem)

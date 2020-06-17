--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Finite.Internal
-- Copyright   :  (C) 2015 mniip
-- License     :  BSD3
-- Maintainer  :  mniip <mniip@mniip.com>
-- Stability   :  experimental
-- Portability :  portable
--------------------------------------------------------------------------------
{-# LANGUAGE KindSignatures, DataKinds, DeriveGeneric #-}
module Data.Finite.Internal
    (
        Finite(Finite),
        finite,
        getFinite,
        natValInt
    )
    where

import GHC.Read
import GHC.TypeLits
import GHC.Generics
import Control.DeepSeq
import Control.Monad
import Data.Ratio
import Text.Read.Lex
import Text.ParserCombinators.ReadPrec

-- | Finite number type. @'Finite' n@ is inhabited by exactly @n@ values. Invariants:
--
-- prop> getFinite x < natVal x
-- prop> getFinite x >= 0
newtype Finite (n :: Nat) = Finite Int
                          deriving (Eq, Ord, Generic)

-- | An Int valued version of natVal
{-# INLINABLE natValInt #-}
natValInt :: KnownNat n => proxy n -> Int
natValInt = fromInteger . natVal

-- | Convert an 'Int' into a 'Finite', throwing an error if the input is out of bounds.
finite :: KnownNat n => Int -> Finite n
finite x = result
    where
        result = if x < natValInt result && x >= 0
            then Finite x
            else error $ "finite: Int " ++ show x ++ " is not representable in Finite " ++ show (natVal result)

-- | Convert a 'Finite' into the corresponding 'Int'.
getFinite :: Finite n -> Int
getFinite (Finite x) = x

-- | Throws an error for @'Finite' 0@
instance KnownNat n => Bounded (Finite n) where
    maxBound = result
        where
            result = if natVal result > 0
                then Finite $ natValInt result - 1
                else error "maxBound: Finite 0 is uninhabited"
    minBound = result
        where
            result = if natVal result > 0
                then Finite 0
                else error "minBound: Finite 0 is uninhabited"

instance KnownNat n => Enum (Finite n) where
    fromEnum = fromEnum . getFinite
    toEnum = finite . toEnum
    enumFrom x = enumFromTo x maxBound
    enumFromThen x y = enumFromThenTo x y (if x >= y then minBound else maxBound)

instance Show (Finite n) where
    showsPrec d (Finite x) = showParen (d > 9) $ showString "finite " . showsPrec 10 x

instance KnownNat n => Read (Finite n) where
    readPrec = parens $ Text.ParserCombinators.ReadPrec.prec 10 $ do 
                 expectP (Ident "finite")
                 x <- readPrec
                 let result = finite x
                 guard (x >= 0 && x < natValInt result) 
                 return result

-- | Modular arithmetic. Only the 'fromInteger' function is supposed to be useful.
instance KnownNat n => Num (Finite n) where
    fx@(Finite x) + Finite y = Finite $ (x + y) `mod` natValInt fx
    fx@(Finite x) - Finite y = Finite $ (x - y) `mod` natValInt fx
    fx@(Finite x) * Finite y = Finite $ (x * y) `mod` natValInt fx
    abs fx = fx
    signum _ = fromInteger 1
    fromInteger x = result
        where
            result = if x < natVal result && x >= 0
                then Finite (fromInteger x)
                else error $ "fromInteger: Int " ++ show x ++ " is not representable in Finite " ++ show (natVal result)

instance KnownNat n => Real (Finite n) where
    toRational (Finite x) = (fromIntegral x) % 1

-- | __Not__ modular arithmetic.
instance KnownNat n => Integral (Finite n) where
    quotRem (Finite x) (Finite y) = (Finite $ x `quot` y, Finite $ x `rem` y)
    toInteger (Finite x) = fromIntegral x

instance NFData (Finite n)

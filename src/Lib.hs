{-# LANGUAGE DataKinds #-}


module Lib where


import Data.Mod (Mod, divisor, op, toMod, value)
import GHC.TypeLits


class Group a where
    gadd :: a -> a -> a
    gzero :: a
    gneg :: a -> a


instance KnownNat a => Group (Mod a) where
    gadd = op (+)
    gzero = toMod 0
    gneg v = op (-) (toMod $ divisor v) v


class Field a where
    add :: a -> a -> a
    prod :: a -> a -> a
    zero :: a
    one :: a
    neg :: a -> a
    inv :: a -> Maybe a


instance KnownNat a => Field (Mod a) where
    add = op (+)
    prod = op (*)
    zero = toMod 0
    one = toMod 1
    neg v = op (-) (toMod $ divisor v) v
    inv v = fmap toMod $ modInv x $ divisor v
        where x = value v


-- Extended Euclidean algorithm.  Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).  Note that x or y may be negative.
gcdExt a 0 = (1, 0, a)
gcdExt a b = (t, s - q * t, g)
    where
        (q, r) = a `quotRem` b
        (s, t, g) = gcdExt b r


-- Given a and m, return Just x such that ax = 1 mod m.  If there is no such x
-- return Nothing.
modInv a m
    | g == 1 = Just $ mkPos i
    | otherwise = Nothing
    where
        (i, _, g) = gcdExt a m
        mkPos x
            | x < 0 = x + m
            | otherwise = x


someFunc :: IO ()
someFunc = do
    print "Field"

    let five = toMod 3 :: Mod 7
        four = toMod 4 :: Mod 7

    print "5 + 4 = 3 | Mod 7"
    print $ add five four

    print "zero + 4 = 4 | Mod 7"
    print $ add zero four

    print "one * 4 = 4 | Mod 7"
    print $ prod one four

    print "4 * 4 = 4 | Mod 7"
    print $ prod four four

    print "4 * inv(4) = 1 | Mod 7"
    print $ fmap (prod four) $ inv four
    print $ inv four

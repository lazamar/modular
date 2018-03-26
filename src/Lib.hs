{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}


module Lib where


import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits


data Mod :: Nat -> * where
    Mod :: KnownNat a => Int -> Mod a


deriving instance Eq (Mod a)
deriving instance Show (Mod a)


toMod :: KnownNat a => Int -> Mod a
toMod v = second x $ Mod (v `mod` modVal x)
    where
        x = Mod v

        second :: b -> b -> b
        second a b = b


modVal :: KnownNat a => proxy a -> Int
modVal = fromIntegral . natVal


op :: KnownNat a => (Int -> Int -> Int) -> Mod a -> Mod a -> Mod a
op f (Mod a) (Mod b) = toMod $ f a b


class Group a where
    gadd :: a -> a -> a
    gzero :: a
    gneg :: a -> a


instance KnownNat a => Group (Mod a) where
    gadd = op (+)
    gzero = Mod 0
    gneg v = toMod $ modVal v - x
        where Mod x = v


class Field a where
    add :: a -> a -> a
    prod :: a -> a -> a
    inv :: a -> Maybe a
    neg :: a -> a
    zero :: a
    one :: a


instance KnownNat a => Field (Mod a) where
    add = op (+)
    prod = op (*)
    neg v = toMod $ modVal v - x
        where Mod x = v
    inv v = fmap toMod $ modInv x $ modVal v
        where Mod x = v
    zero = toMod 0
    one = toMod 1


-- Extended Euclidean algorithm.  Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).  Note that x or y may be negative.
gcdExt a 0 = (1, 0, a)
gcdExt a b = let (q, r) = a `quotRem` b
                 (s, t, g) = gcdExt b r
             in (t, s - q * t, g)


-- Given a and m, return Just x such that ax = 1 mod m.  If there is no such x
-- return Nothing.
modInv a m = let (i, _, g) = gcdExt a m
             in if g == 1 then Just (mkPos i) else Nothing
  where mkPos x = if x < 0 then x + m else x


someFunc :: IO ()
someFunc = do
    print "Field"

    let five = toMod 3 :: Mod 7
        four = toMod 4 :: Mod 7

    print "5 + 4 = 3 :: Mod 7"
    print $ add (toMod 5) (toMod 4 :: Mod 7)

    print "zero + 4 = 4 :: Mod 7"
    print $ add zero four

    print "one * 4 = 4 :: Mod 7"
    print $ prod one four

    print "4 * 4 = 4 :: Mod 7"
    print $ prod four four

    print "4 * inv(4) = 1 :: Mod 7"
    print $ fmap (prod four) $ inv four
    print $ inv four

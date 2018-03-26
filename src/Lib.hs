{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}


module Lib
    ( someFunc
    ) where


import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits


data Mod :: Nat -> * where
    Mod :: KnownNat a => Int -> Mod a


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


instance KnownNat a => Group (Mod a) where
    gadd = op (+)
    gzero = Mod 0


class Field a where
    add :: a -> a -> a
    prod :: a -> a -> a
    neg :: a -> a
    inv :: a -> a
    zero :: a
    one :: a


instance KnownNat a => Field (Mod a) where
    add = op (+)
    prod = op (*)
    neg v = add v . toMod $ -1 * modVal v
    inv v = op gcd v . toMod $ modVal v
    zero = toMod 0
    one = toMod 1


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
    print $ prod four $ inv four
    print $ inv four

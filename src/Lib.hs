{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}


module Lib
    ( someFunc
    ) where


import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits


data Mod :: Nat -> * where
    Mod :: KnownNat a => Integer -> Mod a

deriving instance Show (Mod a)

toMod :: KnownNat a => Integer -> Mod a
toMod v = second x $ Mod (v `mod` natVal x)
    where
        x = Mod v

        second :: b -> b -> b
        second a b = b

op :: (Integer -> Integer -> Integer) -> Mod a -> Mod a -> Mod a
op f (Mod a) (Mod b) = toMod $ f a b

class Group a where
    add :: a -> a -> a
    neutral :: a

instance (KnownNat a) => Group (Mod a) where
    neutral = Mod 0
    add = op (+)


class Field a where
    sum :: a -> a -> a
    prod :: a -> a -> a
    neg :: a -> a
    inv :: a -> a
    zero :: a
    one :: a

-- instance KnownNat a => Field (Mod a) where
--     sum =

a :: Mod 5
a = toMod 6

someFunc :: IO ()
someFunc = print $ a

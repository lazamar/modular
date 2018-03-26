{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}


module Data.Mod (Mod, value, divisor, toMod, op) where


import GHC.TypeLits


data Mod :: Nat -> * where
    Mod :: KnownNat a => Int -> Mod a


deriving instance Eq (Mod a)
deriving instance Show (Mod a)


toMod :: KnownNat a => Int -> Mod a
toMod v = second x $ Mod (v `mod` divisor x)
    where
        x = Mod v

        second :: b -> b -> b
        second a b = b


value :: Mod a -> Int
value (Mod x) = x


divisor :: KnownNat a => proxy a -> Int
divisor = fromIntegral . natVal


op :: KnownNat a => (Int -> Int -> Int) -> Mod a -> Mod a -> Mod a
op f (Mod a) (Mod b) = toMod $ f a b

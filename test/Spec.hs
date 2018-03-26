{-# LANGUAGE DataKinds #-}


import Control.Exception (evaluate)
import Data.Mod (Mod, divisor, op, toMod, value)
import Lib
import Test.Hspec
import Test.QuickCheck


type ModType = Mod 13


main :: IO ()
main = hspec $ do
  describe "Mod" $
    it "applies modulo on value construction" $
        property $
            \x ->
                let
                    a = toMod x :: ModType
                in
                    value a == x `mod` divisor a
  describe "Group" $ do
    it "has an associative gadd operation" $
        property $
            \x y z ->
                let
                    a = toMod x :: ModType
                    b = toMod y :: ModType
                    c = toMod z :: ModType
                in
                    gadd a (add b c) == gadd (add a b) c

    it "has gzero as the identity element" $
        property $
            \x ->
                let
                    a = toMod x :: ModType
                in
                    gadd gzero a == a

    it "has a negation function where a + gneg(a) == zero" $
        property $
            \x ->
                let
                    a = toMod x :: ModType
                in
                    gadd a (gneg a) == gzero

  describe "Field" $ do
    it "has an associative add operation" $
        property $
            \x y z ->
                let
                    a = toMod x :: ModType
                    b = toMod y :: ModType
                    c = toMod z :: ModType
                in
                    add a (add b c) == add (add a b) c

    it "has zero as the neutral element of addition" $
        property $
            \x ->
                let
                    a = toMod x :: ModType
                in
                    add zero a == a

    it "has a negation function that returns the inverse for addition" $
        property $
            \x ->
                let
                    a = toMod x :: ModType
                in
                    add a (neg a) == zero

    it "defines a product function whose return value always belongs to the Field set" $
        property $
            \x y ->
                let
                    a = toMod x :: ModType
                    b = toMod y :: ModType
                    r = prod a b
                in
                    value r == (x * y) `mod` divisor a

    it "has an associative product" $
        property $
            \x y z ->
                let
                    a = toMod x :: ModType
                    b = toMod y :: ModType
                    c = toMod z :: ModType
                in
                    prod a (prod b c) == prod (prod a b) c

    it "has one as the neutral element of multiplication" $
        property $
            \x ->
                let
                    a = toMod x :: ModType
                in
                    prod one a == a

    it "has an inv function that returns the multiplicative inverse" $
        property $
            \x ->
                let
                    a = toMod x :: ModType
                in
                    mod x (divisor a) /= 0 ==>
                    fmap (prod a) (inv a) == Just one

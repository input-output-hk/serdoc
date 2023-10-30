{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.SerDoc.Test.Info
where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.SerDoc.Info

tests :: TestTree
tests = testGroup "Info"
          [ testGroup "FieldSize"
              [ testProperty "KnownFieldSize" pKnownFieldSizeKnown
              , testProperty "FieldSizeWithinExpectedLimits" pFieldSizeWithinExpectedLimits
              ]
          ]

pKnownFieldSizeKnown :: KnownFieldSize -> Property
pKnownFieldSizeKnown (KnownFieldSize _ s) =
  knownSize s =/= Nothing

pFieldSizeWithinExpectedLimits :: AnyFieldSize -> Property
pFieldSizeWithinExpectedLimits (AnyFieldSize size s) =
  let actual = knownSize s
  in
    case actual of
      Nothing -> label "unknown size" $ property True
      Just actualSize -> do
        label "known size" . property $ actualSize <= size

-- * Helpers

data KnownFieldSize = KnownFieldSize Int FieldSize
  deriving (Show, Eq)

data AnyFieldSize = AnyFieldSize Int FieldSize
  deriving (Show, Eq)

instance Arbitrary KnownFieldSize where
  arbitrary = getSize >>= genKnownFieldSize
  shrink (KnownFieldSize size s) = KnownFieldSize size <$> shrinkFieldSize s

instance Arbitrary AnyFieldSize where
  arbitrary = do
    size <- getSize
    AnyFieldSize size <$> genFieldSize False size
  shrink (AnyFieldSize size s) = AnyFieldSize size <$> shrinkFieldSize s

shrinkFieldSize :: FieldSize -> [FieldSize]
shrinkFieldSize UnknownSize =
  []
shrinkFieldSize (VarSize name) =
  VarSize <$> shrink name
shrinkFieldSize (FixedSize n) =
  FixedSize <$> shrink n
shrinkFieldSize (RangeSize a b) =
  fmap (\b' -> RangeSize a b') (shrinkFieldSize b) ++
  fmap (\a' -> RangeSize a' b) (shrinkFieldSize a) ++
  [ a, b ]
shrinkFieldSize (BinopSize op a b) =
  fmap (\b' -> BinopSize op a b') (shrinkFieldSize b) ++
  fmap (\a' -> BinopSize op a' b) (shrinkFieldSize a) ++
  [ a, b ]
shrinkFieldSize EnumSize =
  []


genFieldSize :: Bool -> Int -> Gen FieldSize
genFieldSize onlyKnown n
  = oneof $
      [ return $ FixedSize n ] ++
      [ VarSize <$> arbitrary
      | not onlyKnown
      ] ++
      [ pure UnknownSize
      | not onlyKnown
      ] ++
      [ do
          a <- chooseInt (1, n - 1)
          RangeSize
            <$> genFieldSize onlyKnown a
            <*> genFieldSize onlyKnown (n - 1)
      | n > 1
      , not onlyKnown
      ] ++
      [ do
          a <- chooseInt (1, n - 1)
          BinopSize FSPlus
            <$> genFieldSize onlyKnown a
            <*> genFieldSize onlyKnown (n - a)
      | n >= 1
      ] ++
      [ do
          a <- chooseInt (2, n - 1)
          BinopSize FSMul
            <$> genFieldSize onlyKnown a
            <*> genFieldSize onlyKnown (n `div` a)
      | n >= 2
      ] ++
      [ do
          a <- chooseInt (1, n - 1)
          BinopSize FSMax
            <$> genFieldSize onlyKnown a
            <*> genFieldSize onlyKnown (n - 1)
      | n >= 1
      ] ++
      [ do
          a <- chooseInt (1, n - 1)
          BinopSize FSMax
            <$> genFieldSize onlyKnown (n - 1)
            <*> genFieldSize onlyKnown a
      | n >= 1
      ] ++
      [ do
          a <- chooseInt (1, n - 1)
          BinopSize FSMin
            <$> genFieldSize onlyKnown (n - 1)
            <*> genFieldSize onlyKnown a
      | n >= 1
      ] ++
      [ do
          a <- chooseInt (1, n - 1)
          BinopSize FSMin
            <$> genFieldSize onlyKnown a
            <*> genFieldSize onlyKnown (n - 1)
      | n >= 1
      ]
      

genKnownFieldSize :: Int -> Gen KnownFieldSize
genKnownFieldSize size = KnownFieldSize size <$> genFieldSize True size

genField :: Int -> Gen (FieldInfo codec)
genField 0 = genConstBasicField
genField fuel =
  oneof
    [ genConstBasicField
    , genAnnField fuel
    ]

genConstBasicField :: Gen (FieldInfo codec)
genConstBasicField =
  basicField <$> arbitrary <*> (FixedSize <$> arbitrarySizedNatural)

genAnnField :: Int -> Gen (FieldInfo codec)
genAnnField fuel =
  annField <$> arbitrary <*> genField (fuel - 1)

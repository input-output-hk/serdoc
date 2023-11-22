{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.SerDoc.Binary.Test.Codec
where

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Proxy

import Data.SerDoc.Class
import Data.SerDoc.Info
import Data.SerDoc.Binary.Codec
import Data.Binary.Get (runGetOrFail)
import Data.Binary.Put (runPut)
import Data.Typeable
import Data.Int
import Data.Word
import Numeric.Natural
import qualified Data.ByteString.Lazy as LBS

tests :: TestTree
tests = testGroup "BinaryCodec"
          [ testGroup "Serializable"
              [ testSerializable (Proxy @())
              , testSerializable (Proxy @Integer)
              , testSerializableWith (fromInteger . abs :: Integer -> Natural) (Proxy @Natural)
              , testSerializable (Proxy @Int)
              , testSerializable (Proxy @Int8)
              , testSerializable (Proxy @Int16)
              , testSerializable (Proxy @Int32)
              , testSerializable (Proxy @Int64)
              , testSerializable (Proxy @Word)
              , testSerializable (Proxy @Word8)
              , testSerializable (Proxy @Word16)
              , testSerializable (Proxy @Word32)
              , testSerializable (Proxy @Word64)
              , testSerializable (Proxy @Float)
              , testSerializable (Proxy @Double)
              , testSerializable (Proxy @Ordering)
              , testSerializable (Proxy @[Word8])
              , testSerializable (Proxy @[Double])
              , testSerializable (Proxy @(Maybe Word8))
              , testSerializable (Proxy @(Either Word8 Int32))
              , testSerializable (Proxy @(Word8, Int32))
              , testSerializable (Proxy @(Word8, Integer))
              , testSerializable (Proxy @(Word8, Word16, Word32))
              , testSerializable (Proxy @(Word8, Word16, Word32, Word64))
              , testSerializable (Proxy @(Word8, Word16, Word16, Word32, Word64))
              , testSerializable (Proxy @(Word8, Word16, Word16, Word16, Word32, Word64))
              , testSerializable (Proxy @(Word8, Word16, Word16, Word16, Word16, Word32, Word64))
              , testSerializable (Proxy @(Word8, Word16, Word16, Word16, Word16, Word16, Word32, Word64))
              , testSerializable (Proxy @(Word8, Word16, Word16, Word16, Word16, Word16, Word16, Word32, Word64))
              , testSerializable (Proxy @(Word8, Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word32, Word64))
              ]
          ]

testSerializable :: forall a.
                    ( Typeable a
                    , Arbitrary a
                    , Show a
                    , Eq a
                    , Serializable BinaryCodec a
                    , HasInfo BinaryCodec a
                    )
                 => Proxy a
                 -> TestTree
testSerializable =
  testSerializableWith id

testSerializableWith :: forall a b.
                    ( Typeable b
                    , Arbitrary a
                    , Show a
                    , Show b
                    , Eq b
                    , Serializable BinaryCodec b
                    , HasInfo BinaryCodec b
                    )
                 => (a -> b)
                 -> Proxy b
                 -> TestTree
testSerializableWith make proxy =
  testGroup (show . typeRep $ proxy)
    [ testProperty "Round trip" $ pRoundTrip . make
    , testProperty "Encoded size matches" $ pEncodedSizeMatches . make
    ]

pUnitHasInfo :: (Codec codec, HasInfo codec ())
            => Proxy codec
            -> Property
pUnitHasInfo pCodec =
  actual === expected
  where
    actual = info pCodec (Proxy @())
    expected = basicField "()" (FixedSize 0)

pRoundTrip :: forall a.
              ( Serializable BinaryCodec a
              , Eq a
              , Show a
              )
           => a
           -> Property
pRoundTrip expected =
  case getResult of
    Left (unconsumed, consumed, err) ->
      counterexample ("Error: " ++ err) $
      counterexample ("Unconsumed input: " ++ show unconsumed) $
      counterexample ("Consumed bytes: " ++ show consumed) $
        property False
    Right ("", _size, actual) ->
      expected === actual
    Right (unconsumed, consumed, actual) ->
      counterexample "Not all input consumed" $
      counterexample ("Unconsumed input: " ++ show unconsumed) $
      counterexample ("Consumed bytes: " ++ show consumed) $
      counterexample ("Parsed value: " ++ show actual) $
        property False
  where
    encoded = runPut $ encode (Proxy @BinaryCodec) () expected
    getResult = runGetOrFail (fst <$> decodeM (Proxy @BinaryCodec) () ()) encoded

pEncodedSizeMatches :: forall a.
                       ( Serializable BinaryCodec a
                       , Eq a
                       , Show a
                       , HasInfo BinaryCodec a
                       )
                    => a
                    -> Property
pEncodedSizeMatches value =
  case infoSizeExpr of
    FixedSize s ->
      s === encodedSize
    RangeSize (FixedSize lo) (FixedSize hi) ->
      counterexample (show encoded) $
      counterexample (show infoSizeExpr) $
      counterexample (show encodedSize ++ " < " ++ show lo) (property $ encodedSize >= lo)
      .&&.
      counterexample (show encodedSize ++ " > " ++ show hi) (property $ encodedSize <= hi)
    RangeSize (FixedSize lo) _ ->
      counterexample (show encoded) $
      counterexample (show infoSizeExpr) $
      counterexample (show encodedSize ++ " < " ++ show lo) (property $ encodedSize >= lo)
    RangeSize _ (FixedSize hi) ->
      counterexample (show encoded) $
      counterexample (show infoSizeExpr) $
      counterexample (show encodedSize ++ " > " ++ show hi) (property $ encodedSize <= hi)
    s ->
      label (show s) $
        property True
  where
    encoded = runPut $ encode (Proxy @BinaryCodec) () value
    encodedSize = fromIntegral $ LBS.length encoded
    fi = info (Proxy @BinaryCodec) (Proxy @a)
    infoSizeExpr = fieldSize fi

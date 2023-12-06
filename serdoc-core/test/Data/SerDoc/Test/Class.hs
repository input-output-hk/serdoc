{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.SerDoc.Test.Class
where

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Proxy
import Control.Monad.Identity
import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Monad.Writer
import Control.Monad.State

import Data.SerDoc.Class
import Data.SerDoc.Info
import Data.SerDoc.TH
import Data.SerDoc.TestUtil

data ShowCodec

instance Codec ShowCodec where
  type MonadEncode ShowCodec = Writer ByteString
  type MonadDecode ShowCodec = ExceptT String (State ByteString)

instance HasInfo ShowCodec () where
  info _ _ = basicField "()" (FixedSize $ BS.length $ showBS ())

instance HasInfo ShowCodec Int where
  info _ _ = basicField "Int" (RangeSize (FixedSize 1) (FixedSize $ length $ show (minBound :: Int)))

newtype ViaShow a = ViaShow { viaShow :: a }

instance (Show a, Read a) => Serializable ShowCodec (ViaShow a) where
  encode _ = tell . showBS . viaShow
  decode _ = do
    decodedMay <- readMaybeBS <$> get
    case decodedMay of
      Nothing ->
        throwError "Read: no parse"
      Just (x, rest) -> do
        put rest
        return (ViaShow x)

deriving via (ViaShow ()) instance Serializable ShowCodec ()

deriving via (ViaShow Int) instance Serializable ShowCodec Int


data Record =
  Record
    { firstField :: ()
    , secondField :: Int
    }
    deriving (Show, Read, Eq, Ord)

instance Arbitrary Record where
  arbitrary = Record <$> arbitrary <*> arbitrary
  shrink (Record a b) =
    Record a <$> shrink b

$(deriveSerDoc ''ShowCodec [] ''Record)

data Record1 a =
  Record1
    { firstField1 :: a
    , secondField1 :: [a]
    }
    deriving (Show, Read, Eq, Ord)

instance Arbitrary a => Arbitrary (Record1 a) where
  arbitrary = Record1 <$> arbitrary <*> arbitrary
  shrink (Record1 a b) =
    (Record1 a <$> shrink b)
    ++
    (Record1 <$> shrink a <*> pure b)

$(deriveSerDoc ''ShowCodec [] ''Record1)

tests :: TestTree
tests = testGroup "Class"
          [ testGroup "ShowCodec"
              [ testGroup "HasInfo"
                  [ testProperty "()" $ pUnitHasInfo (Proxy @ShowCodec)
                  ]
              , testGroup "Serializable"
                  [ testProperty "()" $ pRoundTrip @() (Proxy @ShowCodec) execWriter (runState . runExceptT)
                  -- , testProperty "Int" $ pRoundTrip @Int (Proxy @ShowCodec) execWriter runState
                  -- , testProperty "Record" $ pRoundTrip @Record (Proxy @ShowCodec) execWriter runState
                  ]
              ]
          ]

pUnitHasInfo :: (Codec codec, HasInfo codec ())
            => Proxy codec
            -> Property
pUnitHasInfo pCodec =
  actual === expected
  where
    actual = info pCodec (Proxy @())
    expected = basicField "()" (FixedSize $ BS.length $ showBS ())

pRoundTrip :: forall a codec err encoded mdecode.
              ( Codec codec
              , Serializable codec a
              , Arbitrary a
              , Eq a
              , Show a
              , Monad (MonadEncode codec)
              , Monad mdecode
              , MonadDecode codec ~ ExceptT err mdecode
              , Eq encoded
              , Show encoded
              , Monoid encoded
              , Eq err
              , Show err
              )
           => Proxy codec
           -> (MonadEncode codec () -> encoded)
           -> (MonadDecode codec a -> encoded -> (Either err a, encoded))
           -> a
           -> Property
pRoundTrip pCodec runEncode runDecode expected =
  actual === (Right expected, mempty)
  where
    encoded = runEncode (encode pCodec expected)
    actual = runDecode (decode pCodec) encoded

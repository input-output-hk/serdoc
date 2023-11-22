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

module Data.SerDoc.Test.Class
where

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Proxy
import Control.Monad.Identity
import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Data.SerDoc.Class
import Data.SerDoc.Info
import Data.SerDoc.TH
import Data.SerDoc.TestUtil

data ShowCodec

instance Codec ShowCodec where
  type Context ShowCodec = ()
  type MonadEncode ShowCodec = Identity
  type MonadDecode ShowCodec = Except String
  type Encoded ShowCodec = ByteString

instance HasInfo ShowCodec () where
  info _ _ = basicField "()" (FixedSize $ BS.length $ showBS ())

instance HasInfo ShowCodec Int where
  info _ _ = basicField "Int" (RangeSize (FixedSize 1) (FixedSize $ length $ show (minBound :: Int)))

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

$(deriveHasInfo ''ShowCodec ''Record)
$(deriveSerializable ''ShowCodec ''Record)

newtype ViaShow a = ViaShow { viaShow :: a }

instance (Show a, Read a) => Serializable ShowCodec (ViaShow a) where
  encode _ _ = return . showBS . viaShow
  decodeM _ _ str = do
    let decodedMay = readMaybeBS str
    case decodedMay of
      Nothing -> throwError "Read: no parse"
      Just (x, rest) -> return (ViaShow x, rest)

deriving via (ViaShow ()) instance Serializable ShowCodec ()

deriving via (ViaShow Int) instance Serializable ShowCodec Int

tests :: TestTree
tests = testGroup "Class"
          [ testGroup "ShowCodec"
              [ testGroup "HasInfo"
                  [ testProperty "()" $ pUnitHasInfo (Proxy @ShowCodec)
                  ]
              , testGroup "Serializable"
                  [ testProperty "()" $ pRoundTrip @() (Proxy @ShowCodec)
                  , testProperty "Int" $ pRoundTrip @Int (Proxy @ShowCodec)
                  , testProperty "Record" $ pRoundTrip @Record (Proxy @ShowCodec)
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

pRoundTrip :: forall a codec err.
              ( Codec codec
              , Serializable codec a
              , Arbitrary a
              , Eq a
              , Show a
              , MonadEncode codec ~ Identity
              , MonadDecode codec ~ Except err
              , Context codec ~ ()
              , Eq err
              , Show err
              )
           => Proxy codec
           -> a
           -> Property
pRoundTrip pCodec expected =
  actual === Right expected
  where
    encoded = runIdentity $ encode pCodec () expected
    actual = decodeEither pCodec () encoded

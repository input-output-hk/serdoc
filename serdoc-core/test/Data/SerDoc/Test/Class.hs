{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.SerDoc.Test.Class
where

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Proxy
import Control.Monad.Identity
import Text.Read
import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import Data.SerDoc.Class
import Data.SerDoc.Info
import Data.SerDoc.TestUtil

data ShowCodec

instance Codec ShowCodec where
  type Context ShowCodec = ()
  type MonadEncode ShowCodec = Identity
  type MonadDecode ShowCodec = Except String
  type Encoded ShowCodec = ByteString

instance HasInfo ShowCodec () where
  info _ _ = basicField "()" (FixedSize $ BS.length $ showBS ())

instance (Show a, Read a) => Serializable ShowCodec a where
  encode _ _ = return . showBS
  decodeM _ _ str = do
    let decodedMay = readMaybeBS str
    case decodedMay of
      Nothing -> throwError "Read: no parse"
      Just x -> return x

tests :: TestTree
tests = testGroup "Class"
          [ testGroup "ShowCodec"
              [ testGroup "HasInfo"
                  [ testProperty "()" $ pUnitHasInfo (Proxy @ShowCodec)
                  ]
              , testGroup "Serializable"
                  [ testProperty "()" $ pRoundTrip @() (Proxy @ShowCodec)
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

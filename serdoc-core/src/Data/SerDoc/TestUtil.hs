{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.SerDoc.TestUtil
where

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

showBS :: Show a => a -> ByteString
showBS = encodeUtf8 . Text.pack . show

readMaybeBS :: Read a => ByteString -> Maybe a
readMaybeBS = readMaybe . Text.unpack . decodeUtf8

module Data.SerDoc.TestUtil
where

import Text.Read
import Data.ByteString (ByteString)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Maybe

showBS :: Show a => a -> ByteString
showBS = encodeUtf8 . Text.pack . show

readMaybeBS_ :: Read a => ByteString -> Maybe a
readMaybeBS_ = readMaybe . Text.unpack . decodeUtf8

readMaybeBS :: Read a => ByteString -> Maybe (a, ByteString)
readMaybeBS src = do
  (x, restStr) <- listToMaybe . reads . Text.unpack . decodeUtf8 $ src
  return (x, encodeUtf8 . Text.pack $ restStr)

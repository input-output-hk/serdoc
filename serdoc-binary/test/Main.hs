module Main
where

import Test.Tasty

import qualified Data.SerDoc.Binary.Test.Codec as Codec

tests :: TestTree
tests = testGroup "serdoc-binary"
          [ Codec.tests
          ]

main :: IO ()
main = defaultMain tests

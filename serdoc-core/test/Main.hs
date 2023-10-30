module Main
where

import Test.Tasty

import qualified Data.SerDoc.Test.Class as Class
import qualified Data.SerDoc.Test.Info as Info

tests :: TestTree
tests = testGroup "serdoc"
          [ Class.tests
          , Info.tests
          ]

main :: IO ()
main = defaultMain tests

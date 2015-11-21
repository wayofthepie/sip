{-# LANGUAGE
    OverloadedStrings
    #-}

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString

import Test.Tasty
import Test.Tasty.HUnit

import qualified ProcNet as ProcNet

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ProcNet.unitTests]


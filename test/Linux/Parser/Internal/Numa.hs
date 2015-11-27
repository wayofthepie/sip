
module Linux.Parser.Internal.Numa where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Monoid

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Linux.Parser.Internal.Numa


data AllowedNumaMaps = AllowedNumaMaps
    { numaMapsBs     :: ByteString
    , actualNumaMaps :: NumaMap
    } deriving (Eq, Show)


genHex :: Gen ByteString
genHex = listOf1 . elements $ ['a'..'f'] ++ ['0'..'9']

--556cb7d3c000 default file=/usr/bin/bash mapped=225 mapmax=3 N0=225 kernelpagesize_kB=4

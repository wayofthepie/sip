{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 as BC
import Data.Monoid (mempty)
import Test.Framework.Providers.API
import Test.Framework (Test, defaultMainWithOpts, testGroup)
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Runners.Options
import Test.QuickCheck
import Test.QuickCheck.Test

import Linux.Parser.Internal.Common
import Linux.Parser.Internal.Proc


emptyTestOpts = mempty :: TestOptions

testOpts = emptyTestOpts {
    topt_maximum_generated_tests = Just 50000,
    {-
     - This must be higher than topt_maximum_generated_tests. This
     - prevents a bug where large values of topt_maximum_generated_tests
     - will cause the test run to report a failure after 0 tests.
     - -}
     topt_maximum_unsuitable_generated_tests = Just 20000000
}

emptyRunnerOpts = mempty :: RunnerOptions

runnerOpts = emptyRunnerOpts {
    ropt_test_options   = Just testOpts,
    ropt_color_mode     = Just ColorAlways
    }

main = defaultMainWithOpts tests runnerOpts

tests :: [Test]
tests = [
    testProperty "pidp" propAllowedPidp,
    testProperty "intp - neg" propAllowedNegIntp,
    testProperty "intp - pos" propAllowedPosIntp
    ]


-- * Tests for Common

newtype AllowedNegIntp = AllowedNegIntp {
        negInt :: ByteString
    } deriving (Show)


instance Arbitrary AllowedNegIntp where
    arbitrary = fmap AllowedNegIntp negIntByteString 


newtype AllowedPosIntp = AllowedPosIntp {
       posInt :: ByteString
    } deriving (Show)


instance Arbitrary AllowedPosIntp where
    arbitrary = fmap AllowedPosIntp posIntByteString

propAllowedNegIntp :: AllowedNegIntp -> Bool
propAllowedNegIntp (AllowedNegIntp x) = run intp x == Just x

propAllowedPosIntp :: AllowedPosIntp -> Bool
propAllowedPosIntp (AllowedPosIntp x) = run intp x == Just x

-- * Tests for Proc

newtype AllowedPidp = AllowedPidp {
        pid :: ByteString 
    } deriving (Show)

instance Arbitrary AllowedPidp where
    arbitrary = fmap AllowedPidp posIntByteString


propAllowedPidp :: AllowedPidp -> Bool
propAllowedPidp (AllowedPidp x) = run pidp x == Just x 


-- | Helper functions
run :: Show a => Parser a -> ByteString -> Maybe a
run p input =
    case parseOnly p input of
        Left err    -> error err
        Right x     -> Just x

posIntByteString :: Gen ByteString 
posIntByteString = liftA pack $ listOf1 . elements $ ['0'..'9']

negIntByteString :: Gen ByteString
negIntByteString = liftA ( negate . pack ) $
       listOf1 . elements $ ['0'..'9']
    where negate = append "-"



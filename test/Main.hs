{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8
import Data.Monoid (mempty)
import Test.Framework.Providers.API
import Test.Framework (Test, defaultMainWithOpts, testGroup)
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Runners.Options
import Test.QuickCheck
import Test.QuickCheck.Test
import Linux.Parser.Internal.Proc


emptyTestOpts = mempty :: TestOptions

testOpts = emptyTestOpts {
    topt_maximum_generated_tests = Just 500000,
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

newtype AllowedPsval = AllowedPsval {
        unwrapByteString :: ByteString
    } deriving (Show)

instance Arbitrary AllowedPsval where
    arbitrary = AllowedPsval <$>
        ( pack <$> ( listOf1 $ elements $
            ['a'..'z'] ++ ['A'..'Z'] ++ ['(', ')', '-']) )


-- | Property tests for psval
propAllowedPsval :: AllowedPsval -> Bool
propAllowedPsval (AllowedPsval x) = run psval x == Just x


-- | Helper functions
run :: Show a => Parser a -> ByteString -> Maybe a
run p input =
    case parseOnly p input of
        Left err    -> error err
        Right x     -> Just x

tests :: [Test]
tests = [
    testProperty "psval" propAllowedPsval
    ]


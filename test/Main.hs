{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Linux.Parser.Internal.Proc


main = defaultMain tests

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


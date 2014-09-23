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

newtype AllowedByteString = AllowedByteString { 
        unwrapByteString :: ByteString 
    } deriving (Show)

instance Arbitrary ByteString where
    arbitrary = pack <$> ( listOf1 $ elements $     
                    ['a'..'z'] ++ ['A'..'Z'] ++ ['(', ')', '-'])

-- | Property tests for procStatp 
propCharsAllowed :: ByteString -> Bool
propCharsAllowed x = run psval x == Just x


run :: Show a => Parser a -> ByteString -> Maybe a
run p input = 
    case parseOnly p input of
        Left err    -> error err 
        Right x     -> Just x

tests :: [Test]
tests = [
    testProperty "psval" propCharsAllowed
    ]


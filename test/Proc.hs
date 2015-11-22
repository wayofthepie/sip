{-# LANGUAGE
    OverloadedStrings
    #-}
module Proc where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString

import Test.Tasty
import Test.Tasty.HUnit

import Linux.Parser.Internal.Proc


unitTests :: TestTree
unitTests =
    testGroup "Unit Tests"
        [ testCase "Test /proc/pid/maps parser" $
            parserTest mapsp
                "test/data/proc_pid_maps"
                verifyMapspData expectedMapspData
                "Parsing test/data/proc_pid_maps failed!"

        , testCase "Test /proc/meminfo parser" $
            parserTest meminfop
                "test/data/proc_meminfo"
                verifyMemInfop
                expectedMemInfopData
                "Parsing test/data/proc_meminfo failed!"
        ]

parserTest ::
    Parser a
    -> String
    -> (a -> a -> IO ())
    -> a
    -> String
    -> IO ()
parserTest p file verify expected onFail = do
    actual <- runParser p file
    case actual of
        Right a -> verifyParserOutput verify expected a
        Left _  -> assertFailure onFail


runParser :: Parser a -> String -> IO ( Either () a )
runParser parser file = do
    bs <- BS.readFile file
    let eitherRes = parseOnly parser bs
    case eitherRes of
        Right result -> return $ Right result
        Left _       -> return $ Left ()

verifyParserOutput :: (a -> a -> IO ()) -> a -> a -> IO ()
verifyParserOutput f expected actual = f actual expected

verifyListData :: (a -> a -> IO ()) -> [a] -> [a] -> IO ()
verifyListData f actual expected =
    sequence_ $ fmap (\(a, e) -> f a e) $ zip actual expected


-------------------------------------------------------------------------------
-- Tests for /proc/meminfo parser.
-------------------------------------------------------------------------------
verifyMemInfop :: [MemInfo] -> [MemInfo] -> IO ()
verifyMemInfop actual expected =
    verifyListData verifyMemInfop' actual expected
  where
    verifyMemInfop' a e = do
        assertEqual "Parameter incorrect" (_miParam a) (_miParam e)
        assertEqual "Size incorrect" (_miSize a) (_miSize e)
        assertEqual "Qualifier incorrect" (_miQualifier a) (_miQualifier e)

expectedMemInfopData :: [MemInfo]
expectedMemInfopData = [ MemInfo
    { _miParam      = "MemTotal"
    , _miSize       = "16372136"
    , _miQualifier  = Just "kB"
    }, MemInfo
    { _miParam      = "HugePages_Rsvd"
    , _miSize       = "0"
    , _miQualifier  = Nothing
    }, MemInfo
    { _miParam      = "HugePages_Surp"
    , _miSize       = "0"
    , _miQualifier  = Nothing
    }, MemInfo
    { _miParam      = "Hugepagesize"
    , _miSize       = "2048"
    , _miQualifier  = Just "kB"
    }, MemInfo
    { _miParam      = "DirectMap2M"
    , _miSize       = "14036992"
    , _miQualifier  = Just "kB"
    }]

-------------------------------------------------------------------------------
-- Tests for /proc/[pid]/stat parser.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Tests for /proc/[pid]/maps parser.
-------------------------------------------------------------------------------
verifyMapspData :: [MappedMemory] -> [MappedMemory] -> IO ()
verifyMapspData actual expected =
    verifyListData verifyMapspData' actual expected
  where
    verifyMapspData' a e = do
        assertEqual "Memory address incorrect"  (_address a) (_address e)
        assertEqual "Permissions incorrect"     (_perms a) (_perms e)
        assertEqual "Offset incorrect"          (_offset a) (_offset e)
        assertEqual "Device incorrect"      (_dev a) (_dev e)
        assertEqual "Inode incorrect"       (_inode a) (_inode e)
        assertEqual "Pathname incorrect"    (_pathname a) (_pathname e)


expectedMapspData :: [MappedMemory]
expectedMapspData = [ MappedMemory
    { _address  = ("5650fe94b000", "5650fe94c000")
    , _perms    = "rw-p"
    , _offset   = "00003000"
    , _dev      = ("fd", "04")
    , _inode    = "16950326"
    , _pathname = Just "/usr/bin/abrt-watch-log"
    }, MappedMemory
    { _address  = ("7fd26b5b0000", "7fd26b5b2000")
    , _perms    = "rw-p"
    , _offset   = "00000000"
    , _dev      = ("00", "00")
    , _inode    = "0"
    , _pathname = Nothing
    }, MappedMemory
    { _address  = ("ffffffffff600000", "ffffffffff601000")
    , _perms    = "r-xp"
    , _offset   = "00000000"
    , _dev      = ("00", "00")
    , _inode    = "0"
    , _pathname = Just "[vsyscall]"
    }]


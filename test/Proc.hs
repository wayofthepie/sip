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
        [ testCaseSteps "Test /proc/net/dev parser" $ \step -> do
            step "Parse file"
            runMapsp >>= \result -> case result of
                Right mms -> do
                    step "Verifying parsed maps"
                    sequence_ $ map (\(a, e) -> verifyMapspData a e) $ zip mms expectedMapspData
                Left _ -> assertFailure "Parsing test/data/proc_pid_maps failed!"
        ]

verifyMapspData :: MappedMemory -> MappedMemory -> IO ()
verifyMapspData actual expected = do
    assertEqual "Memory address incorrect"  (_address actual) (_address expected)
    assertEqual "Permissions incorrect"     (_perms actual) (_perms expected)
    assertEqual "Offset incorrect"          (_offset actual) (_offset expected)
    assertEqual "Device incorrect"      (_dev actual) (_dev expected)
    assertEqual "Inode incorrect"       (_inode actual) (_inode expected)
    assertEqual "Pathname incorrect"    (_pathname actual) (_pathname expected)


runMapsp :: IO ( Either () [MappedMemory] )
runMapsp = do
    bs <- BS.readFile "test/data/proc_pid_maps"
    let out = parseOnly mapsp bs
    case out of
        Right list-> return $ Right list
        Left _    -> return $ Left ()

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


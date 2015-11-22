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
        [ testCase "Test /proc/[pid]/maps parser" $
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

        , testCase "Test /proc/[pid]/stat parser" $
            parserTest procstatp
                "test/data/proc_pid_stat"
                verifyProcStatpData
                expectedProcStatpData
                "Parsing test/data/proc_pid_stat failed!"
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
-- TODO : Finish!
verifyProcStatpData :: ProcessStat -> ProcessStat -> IO ()
verifyProcStatpData actual expected = do
    assertEqual "Pid incorrect"     (_pid actual) (_pid expected)
    assertEqual "Command incorrect" (_comm actual) (_comm expected)
    assertEqual "State incorrect"   (_state actual) (_state expected)
    assertEqual "Parent pid incorrect" (_ppid actual) (_ppid expected)
    assertEqual "Parent group incorrect" (_pgrp actual) (_pgrp expected)
    assertEqual "Session incorrect" (_session actual) (_session expected)
    assertEqual "Controlling terminal incorrect" (_tty_nr actual) (_tty_nr expected)


expectedProcStatpData :: ProcessStat
expectedProcStatpData = ProcessStat
    { _pid      = "1"
    , _comm     = "(bash)"
    , _state    = "S"
    , _ppid     = "0"
    , _pgrp     = "1"
    , _session  = "1"
    , _tty_nr   = "34822"
    , _tpgid    = "1"
    , _flags    = "4210944"
    , _minflt   = "304711"
    , _cminflt  = "84696478"
    , _majflt   = "288"
    , _cmajflt  = "12436"
    , _utime    = "410"
    , _stime    = "104"
    , _cutime   = "386473"
    , _cstime   = "22566"
    , _priority = "20"
    , _nice     = "0"
    , _num_threads  = "1"
    , _itrealvalue  = "0"
    , _starttime    = "89389"
    , _vsize        = "121729024"
    , _rss          = "1057"
    , _rsslim       = "18446744073709551615"
    , _startcode    = "94766545403904"
    , _endcode      = "94766546418740"
    , _startstack   = "140722636490944"
    , _kstkesp      = "140722636486088"
    , _kstkeip      = "140181753122704"
    , _signal       = "0"
    , _blocked      = "0"
    , _sigignore    = "3670020"
    , _sigcatch     = "1266777851"
    , _wchan        = "18446744071579760626"
    , _nswap        = "0"
    , _cnswap       = "0"
    , _exiti_signal = "17"
    , _processor    = "5"
    , _rt_priority  = "0"
    , _policy       = "0"
    , _delayacct_blkio_ticks = "125"
    , _guest_time   = "0"
    , _cguest_time  = "0"
    , _start_data   = "94766548516424"
    , _end_data     = "94766548563304"
    , _start_brk    = "94766576685056"
    , _arg_start    = "140722636496675"
    , _arg_end      = "140722636496680"
    , _env_start    = "140722636496680"
    , _env_end      = "140722636496874"
    , _exit_code    = "0"
    }

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


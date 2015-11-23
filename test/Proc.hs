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
    testGroup "Proc Unit Tests"
        [ testCase "Test /proc/[pid]/maps parser" testMapsp
        , testCase "Test /proc/meminfo parser" testMemInfop
        , testCase "Test /proc/[pid]/stat parser" testProcStatp
        , testCase "Test /proc/[pid]/statm parser" testStatmp
        , testCase "Test /proc/loadavg parser" testLoadAvgp
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
testMemInfop :: IO ()
testMemInfop =
    let verify = verifyListData testMemInfop'
    in  parserTest meminfop
            "test/data/proc_meminfo"
            verify
            expectedMemInfopData
            "Parsing test/data/proc_meminfo failed!"
  where
    testMemInfop' a e = do
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
testProcStatp :: IO ()
testProcStatp = parserTest procstatp
    "test/data/proc_pid_stat"
    verify
    expectedProcStatpData
    "Parsing test/data/proc_pid_stat failed!"
  where
    verify a e = assertEqual
        "/proc/[pid]/stat parser returned incorrect value!" a e


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
-- Tests for /proc/loadavg parser.
-------------------------------------------------------------------------------
testLoadAvgp :: IO ()
testLoadAvgp = parserTest loadavgp
    "test/data/proc_loadavg"
    verify
    expectedLoadAvgpData
    "Parsing test/data/proc_loadavg failed!"
  where
    verify actual expected = do
        assertEqual "1 min run queue incorrect" (_runQLen1 actual) (_runQLen1 expected)
        assertEqual "5 min run queue incorrect" (_runQLen5 actual) (_runQLen5 expected)
        assertEqual "15 min run queue incorrect" (_runQLen15 actual) (_runQLen15 expected)
        assertEqual "runnable incorrect" (_runnable actual) (_runnable expected)
        assertEqual "exists incorrect" (_exists actual) (_exists expected)
        assertEqual "latest pid incorrect" (_latestPid actual) (_latestPid expected)


expectedLoadAvgpData :: LoadAvg
expectedLoadAvgpData = LoadAvg
    { _runQLen1 = "0.20"
    , _runQLen5 = "0.15"
    , _runQLen15= "0.07"
    , _runnable = "1"
    , _exists   = "537"
    , _latestPid= "163"
    }

-------------------------------------------------------------------------------
-- Tests for /proc/[pid]/statm parser.
-------------------------------------------------------------------------------
testStatmp :: IO ()
testStatmp = parserTest statmp
    "test/data/proc_pid_statm"
    verify
    expectedStatmpData
    "Parsing test/data/proc_pid_statm failed!"
  where
    verify a e = do
        assertEqual "Size incorrect" (_size a) (_size e)
        assertEqual "Resident size incorrect" (_resident a) (_resident e)
        assertEqual "Share incorrect" (_share a) (_share e)
        assertEqual "Text incorrect" (_text a) (_text e)
        assertEqual "Lib incorrect" (_lib a) (_lib e)
        assertEqual "Data incorrect" (_data a) (_data e)
        assertEqual "Dt incorrect" (_dt a) (_dt e)


expectedStatmpData :: Statm
expectedStatmpData = Statm
    { _size     = "40453"
    , _resident = "2582"
    , _share    = "1395"
    , _text     = "570"
    , _lib      = "0"
    , _data     = "1190"
    , _dt       = "0"
    }

-------------------------------------------------------------------------------
-- Tests for /proc/[pid]/maps parser.
-------------------------------------------------------------------------------
testMapsp :: IO ()
testMapsp =
    let verify = verifyListData testMapsp'
    in parserTest mapsp
                "test/data/proc_pid_maps"
                verify
                expectedMapspData
                "Parsing test/data/proc_pid_maps failed!"
  where
    testMapsp' a e = do
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


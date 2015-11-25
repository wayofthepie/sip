{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
    Module          : Linux.Parser.Internal.Proc
    Descripition    : Parsers for the /proc vfs.

    The examples below use the __io-streams__ library.
-}

module Linux.Parser.Internal.Proc
    ( -- * Data Types
    -- ** MappedMemory : \/proc\/[pid]\/maps
      MappedMemory (..)

    -- ** Limits : \/proc\/[pid]\/limits
    , Limit ()

    -- ** Statm : \/proc\/[pid]\/statm
    , Statm (..)

    -- ** MountInfo : \/proc\/[pid]\/mountinfo
    , MountInfo ()

    -- * MemInfo : \/proc\/meminfo
    , MemInfo (..)

    -- * ProcessStat : \/proc\/\[pid\]\/stat
    , ProcessStat (..)
    , LoadAvg (..)
    , Uptime (..)
    , ProcIO (..)

    -- * Parsers
    , meminfop
    , loadavgp
    , uptimep
    , commp
    , iop
    , mapsp
    , environp
    , procstatp
    , statmp
    , numamapsp
    , limitsp
    , mountInfop
    ) where

import Control.Applicative hiding (empty)
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import Data.Maybe
import Data.ByteString hiding (takeWhile, count, foldl)
import qualified Data.Map as Map

import Prelude hiding (takeWhile)

import Linux.Parser.Internal.Common

-------------------------------------------------------------------------------
-- | Data type for __\/proc\/[pid]\/maps__.
data MappedMemory = MappedMemory
    { _address :: (ByteString, ByteString)
      -- ^ Memory address in the form (start-address, end-address)

    , _perms   :: ByteString
    , _offset  :: ByteString

    , _dev     :: (ByteString, ByteString)
      -- ^ Device number in the form (Major num, Minor num)

    , _inode   :: ByteString
    , _pathname:: Maybe ByteString
    } deriving (Eq, Show)


-- | Data type for __\/proc\/[pid]\/limits__.
data Limit = Limit
    { _limit  :: [ByteString]
    , _slimit :: ByteString
    , _hlimit :: ByteString
    , _unit   :: Maybe ByteString
    } deriving (Eq, Show)


-- | Data type for __\/proc\/[pid]\/statm__.
data Statm = Statm
    { _size       :: ByteString
    , _resident   :: ByteString
    , _share      :: ByteString
    , _text       :: ByteString
    , _lib        :: ByteString
    , _data       :: ByteString
    , _dt         :: ByteString
    } deriving (Eq, Show)



-- Data type for __\/proc\/[pid]\/mountinfo__.
data MountInfo = MountInfo
    { _mountid        :: ByteString
    , _parentid       :: ByteString
    , _devmajmin      :: (ByteString, ByteString)
    , _root           :: ByteString
    , _mountpoint     :: ByteString
    , _mountopts      :: [ByteString]
    , _optionalfields :: Maybe [(ByteString, ByteString)]
    , _fstype         :: ByteString
    , _mountsource    :: ByteString
    , _superoptions   :: [ByteString]
    } deriving (Eq, Show)


data MemInfo = MemInfo
    { _miParam     :: ByteString
    , _miSize      :: ByteString
    , _miQualifier :: Maybe ByteString
    } deriving (Eq, Show)


-- | Data type for \/proc\/[pid]\/stat. See __man_proc__ for more in
-- depth information.
data ProcessStat = ProcessStat
    { _pid        :: ByteString     -- ^ PID
    , _comm       :: ByteString  -- ^ File name of executable
    , _state      :: ByteString  -- ^ Processes state
    , _ppid       :: ByteString  -- ^ Parent processes PID
    , _pgrp       :: ByteString  -- ^ Process group ID
    , _session    :: ByteString  -- ^ Session ID
    , _tty_nr     :: ByteString -- ^ Controlling terminal

    -- | Foreground process group ID of controlling terminal
    , _tpgid      :: ByteString
    , _flags      :: ByteString -- ^ Kernel flags word

    -- | The number of minor faults the process has made which have not
    -- required loading a memory page from disk.
    , _minflt     :: ByteString

    -- | The number of minor faults that the process's waited-for children
    -- have made.
    , _cminflt    :: ByteString

    -- | The number of major faults the process has made which have
    -- required loading a memory page from disk.
    , _majflt     :: ByteString

    -- |The number of major faults that the process's waited-for children
    -- have made.
    , _cmajflt    :: ByteString

    -- | Amount of time, in clock ticks, scheduled in user mode.
    , _utime      :: ByteString

    -- | Amount of time, in clock ticks, that this process's waited-for
    -- children have been scheduled in user  mode.
    , _stime      :: ByteString

    -- | Amount of time that this process's waited-for children have
    -- been scheduled in user  mode,  measured  in  clock  ticks
    -- (divide  by sysconf(_SC_CLK_TCK)).
    , _cutime     :: ByteString


    -- | Amount  of  time  that  this  process's waited-for children
    -- have been scheduled in kernel mode, measured in clock ticks
    -- (divide by sysconf(_SC_CLK_TCK)).
    , _cstime     :: ByteString

    , _priority   :: ByteString -- ^ Processes prority
    , _nice       :: ByteString -- ^ Processes nice value
    , _num_threads:: ByteString -- ^ Number of threads in this process

    -- | The  time  in jiffies before the next SIGALRM is sent to the
    -- process due to an interval timer.
    , _itrealvalue:: ByteString

    -- | Time, in clock ticks, the process started after system boot.
    , _starttime  :: ByteString

    , _vsize      :: ByteString -- ^ Virtual memory size in bytes.
    , _rss        :: ByteString -- ^ Resident set size.

    -- | Current soft limit in bytes on the rss of the process.
    , _rsslim     :: ByteString

    -- | Address above which program text can run.
    , _startcode  :: ByteString

    -- | Address below which program text can run.
    , _endcode    :: ByteString


    , _startstack :: ByteString -- ^ Address of the start of the stack.
    , _kstkesp    :: ByteString -- ^ Current value of ESP (stack pointer).
    , _kstkeip    :: ByteString -- ^ Current EIP (instruction pointer).

    -- | The bitmap of pending signals, displayed as a decimal number.
    -- Obsolete, use __\/proc\/[pid]\/status__ instead.
    , _signal     :: ByteString

    -- |  The bitmap of blocked signals, displayed as a decimal number.
    -- Obsolete, use __\/proc\/[pid]\/status__ instead.
    , _blocked    :: ByteString

    -- | The bitmap of ignored signals, displayed as a decimal number.
    -- Obsolete, use __\/proc\/[pid]\/status__ instead.
    , _sigignore  :: ByteString

    -- | The  bitmap of caught signals, displayed as a decimal number.
    -- Obsolete, use __\/proc\/[pid]\/status__ instead.
    , _sigcatch   :: ByteString

    , _wchan      :: ByteString -- ^ Channel which the process is waiting.

    -- | Number of pages swapped (not maintained).
    , _nswap      :: ByteString

    -- | Cumulative __nswap__ for child processes (not maintained).
    , _cnswap     :: ByteString

    -- | Signal to be sent to parent when process dies.
    , _exiti_signal :: ByteString

    , _processor  :: ByteString -- ^ CPU number last executed on.
    , _rt_priority:: ByteString -- ^ Real-time scheduling priority.
    , _policy     :: ByteString -- ^ Scheduling policy.

    -- | Aggregated block IO delays.
    , _delayacct_blkio_ticks :: ByteString

    -- |  Guest time of the process (time spent running a virtual CPU for
    -- a guest operating system), measured  in  clock  ticks  (divide  by
    -- sysconf(_SC_CLK_TCK)).
    , _guest_time :: ByteString

    -- | Guest time of the process's children, measured in clock ticks
    -- (divide by sysconf(_SC_CLK_TCK)).
    , _cguest_time:: ByteString

    -- | Address above which program initialized and uninitialized (BSS)
    -- data are placed.
    , _start_data :: ByteString

    -- | Address below which program initialized and uninitialized (BSS)
    -- data are placed.
    , _end_data   :: ByteString

    -- | Address above which program heap can be expanded with brk(2).
    , _start_brk  :: ByteString


    -- | Address above which program command-line arguments (argv) are placed.
    , _arg_start  :: ByteString

    -- | Address below program command-line arguments (argv) are placed.
    , _arg_end    :: ByteString

    -- | Address above which program environment is placed.
    , _env_start  :: ByteString

    -- | Address below which program environment is placed
    , _env_end    :: ByteString

    -- | The thread's exit status in the form reported by waitpid(2).
    , _exit_code  :: ByteString
    } deriving (Eq, Show)


-------------------------------------------------------------------------------
-- | Parser for __\/proc\/meminfo__.
--
-- @
--  openFile "\/proc\/meminfo" ReadMode >>=
--      \\h -> handleToInputStream h >>=
--          \\is -> parseFromStream  meminfop is
--
-- [("MemTotal","4052076",Just "kB"),("MemFree","3450628",Just "kB"), ...]
-- @
meminfop :: Parser [MemInfo] --[(ByteString, ByteString, Maybe ByteString)]
meminfop = manyTill ( MemInfo
    <$> idp
    <*> ( skipJustSpacep *> intp <* skipJustSpacep )
    <*> unitp <* skipMany space) endOfInput



-------------------------------------------------------------------------------
-- | Parser for __\/proc\/[pid]\/stat__.
--
-- @
--  openFile "\/proc\/1\/stat" ReadMode >>=
--     \\h -> handleToInputStream h >>=
--          \\is -> parseFromStream procstatp is
--
--  ["1","(systemd)",\"S\","0","1", ...]
-- @
procstatp :: Parser ProcessStat --[ByteString]
procstatp = ProcessStat <$>
    psval <*> psval <*> psval <*> psval <*> psval
    <*> psval <*> psval <*> psval <*> psval <*> psval
    <*> psval <*> psval <*> psval <*> psval <*> psval
    <*> psval <*> psval <*> psval <*> psval <*> psval
    <*> psval <*> psval <*> psval <*> psval <*> psval
    <*> psval <*> psval <*> psval <*> psval <*> psval
    <*> psval <*> psval <*> psval <*> psval <*> psval
    <*> psval <*> psval <*> psval <*> psval <*> psval
    <*> psval <*> psval <*> psval <*> psval <*> psval
    <*> psval <*> psval <*> psval <*> psval <*> psval
    <*> psval <*> psval


psval ::  Parser ByteString
psval = ( takeWhile ( inClass "a-zA-z0-9()-" ) <* space )



------------------------------------------------------------------------------
-- | Parser for __\/proc\/[pid]\/statm__.
--
-- @
--  openFile "/proc/1/statm" ReadMode >>=
--      \\h -> handleToInputStream h >>=
--          \\is -> parseFromStream  (statmp) is
--
--  Statm {_size = "6490", _resident = "1143", ...]
-- @
statmp :: Parser Statm
statmp = Statm
    <$> parseVal <*> parseVal
    <*> parseVal <*> parseVal
    <*> parseVal <*> parseVal
    <*> parseVal
    where
        parseVal :: Parser ByteString
        parseVal = ( takeWhile isDigit ) <* skipJustSpacep



------------------------------------------------------------------------------
-- | Parser for __\/proc\/loadavg__. Only parses the 1, 5 and 15 minute load
-- average, discards the fourth and fifth fields (kernel scheduling entities
-- and latest PID assigned).
--
-- @
--  openFile "\/proc\/loadavg" ReadMode >>=
--      \\h -> handleToInputStream h >>=
--          \\is -> parseFromStream loadavgp is
--
--  ("0.00","0.01","0.05")
-- @

data LoadAvg = LoadAvg
    { _runQLen1 :: ByteString
    , _runQLen5 :: ByteString
    , _runQLen15:: ByteString
    , _runnable :: ByteString
    , _exists   :: ByteString
    , _latestPid:: ByteString
    } deriving (Eq, Show)

loadavgp :: Parser LoadAvg
loadavgp = LoadAvg <$>
    doublep
    <*> doublep
    <*> doublep
    <*> intp <* char '/' <* skipJustSpacep
    <*> intp <* skipJustSpacep
    <*> intp


------------------------------------------------------------------------------
-- | Parser for __\/proc\/uptime__. The first field is the uptime of the system
-- (seconds), the second is the amount of time spent in the idle process
-- (seconds).
--
-- @
--  openFile "\/proc\/uptime" ReadMode >>=
--      \\h -> handleToInputStream h >>=
--          \\is -> parseFromStream uptimep is
--
--  ("13048.12","78085.17")
-- @

data Uptime = Uptime
    { _upSec    :: ByteString
    , _idleTime :: ByteString
    } deriving (Eq,Show)

uptimep :: Parser Uptime
uptimep = Uptime <$> doublep <*> doublep


------------------------------------------------------------------------------
-- | Parser for __\/proc\/[pid]\/comm__.
--
-- @
--  openFile "\/proc\/1\/comm" ReadMode >>=
--      \\h -> handleToInputStream h >>=
--          \\is -> parseFromStream (commp) is
--
--  "systemd"
-- @
commp :: Parser ByteString
commp = takeWhile ( inClass "a-zA-Z0-9:/" )



-------------------------------------------------------------------------------
-- | Parser for __\/proc\/[pid]\/io__.
--
-- @
--  openFile "\/proc\/1\/io" ReadMode >>=
--      \\h -> handleToInputStream h >>=
--          \\is -> parseFromStream (iop) is
--
--  [("rchar","12983399"),("wchar","14957379"), ...]
--
-- @

data ProcIO = ProcIO
    { _rchar        :: ByteString
    , _wchar        :: ByteString
    , _syscr        :: ByteString
    , _syscw        :: ByteString
    , _readBytes    :: ByteString
    , _writeBytes   :: ByteString
    , _cancelledWriteBytes :: ByteString
    } deriving (Eq, Show)

iop :: Parser ProcIO
iop = ProcIO <$> rowp <*> rowp <*> rowp <*> rowp
    <*> rowp <*> rowp <*> rowp <* endOfInput
  where
    rowp = idp *> ( skipJustSpacep *> intp <* skipMany space )


-------------------------------------------------------------------------------
-- | Parser for __\/proc\/[pid]\/maps__.
--
-- @
--  openFile "\/proc\/1\/maps" ReadMode >>=
--      \\h -> handleToInputStream h >>=
--          \\is -> parseFromStream mapsp is
--
--  [MappedMemory {_address = ("7f9c51069000","7f9c5107e000"), _perms = "r-xp", ...]
-- @
mapsp :: Parser [MappedMemory]
mapsp = manyTill ( mapsrowp <* endOfLine ) endOfInput


-- | Parse a row of \/proc\/[pid]\/maps
mapsrowp :: Parser MappedMemory
mapsrowp = MappedMemory
    <$> addressp <* skipJustSpacep
    <*> permp <* skipJustSpacep
    <*> hdp <* skipJustSpacep
    <*> devicep <* skipJustSpacep
    <*> intp <* skipJustSpacep
    <*> pathnamep
    where
        addressp :: Parser (ByteString, ByteString)
        addressp = (,) <$> ( hdp <* char '-' ) <*> hdp

        permp :: Parser ByteString
        permp = takeWhile $ inClass "-rwxp"

        devicep :: Parser (ByteString, ByteString)
        devicep = (,) <$> ( hdp <* char ':' ) <*> hdp

        pathnamep :: Parser (Maybe ByteString)
        pathnamep = peekChar >>= \c -> case c of
                Just '\n'   -> return Nothing
                _           -> liftA Just $
                                takeTill ( inClass "\n" )



-----------------------------------------------------------------------------
-- | Parser for __\/proc\/[pid]\/environ__.
--
-- @
--  openFile "/proc/373/environ" ReadMode >>=
--      \\h -> handleToInputStream h >>=
--          \\is -> parseFromStream environp is
--
--  [("LANG","en_IE.UTF-8"),("PATH","/usr/local/sbin"), ...]
-- @

environp :: Parser (Map.Map ByteString ByteString) --[(ByteString, ByteString)]
environp = Map.fromList <$> ( sepBy environrowp $ char '\NUL' )

environrowp :: Parser (ByteString, ByteString)
environrowp = (,) <$>
    ( ( takeTill $ inClass "=" ) <* char '=' )
    <*> ( takeTill $ inClass "\NUL" )



-----------------------------------------------------------------------------
-- | Parser for \/proc\/[pid]\/numa_maps. Generates a list of 3-tuples :
-- (start addr of mem range, mem policy for range, extra info on pages in
-- range).
--
-- @
--  openFile "/proc/1/numa_maps" ReadMode >>=
--      \\h -> handleToInputStream h >>=
--          \\is -> parseFromStream numamapsp is
--
-- [("7f9c51069000","default",["file=/usr/lib/libz.so.1.2.8", ...]
-- @
numamapsp :: Parser [(ByteString, ByteString, [ByteString])]
numamapsp = manyTill ( (,,)
    <$> ( hdp <* skipJustSpacep )
    <*> ( takeWhile ( inClass "a-zA-Z" ) <* skipJustSpacep )
    <*> ( sepBy ( takeWhile $ inClass "-a-zA-Z0-9=/." ) $ char ' ' )
    <*  endOfLine ) endOfInput



-----------------------------------------------------------------------------
-- | Parser for \/proc\/[pid]\/limits.
--
-- @
--  openFile "/proc/1/limits" ReadMode >>=
--      \\h -> handleToInputStream h >>=
--          \\is -> parseFromStream limitsp is
--
--  [Limits {_limit = ["Max","cpu","time"], _slimit = "unlimited"
-- @
limitsp :: Parser [Limit]
limitsp = parseHeaders *> sepBy limitrowp endOfLine
    where
        parseHeaders :: Parser ByteString
        parseHeaders =
            takeTill (\c -> if c == '\n' then True else False) <* endOfLine


limitrowp :: Parser Limit
limitrowp = Limit
    <$> limitnamep  <* skipJustSpacep
    <*> shlimitp    <* skipJustSpacep
    <*> shlimitp    <* skipJustSpacep
    <*> lunitp      <* skipJustSpacep


limitnamep :: Parser [ByteString]
limitnamep = manyTill ( takeWhile ( inClass "a-zA-Z" ) <* char ' ' ) $ char ' '


shlimitp :: Parser ByteString
shlimitp = takeWhile $ inClass "a-zA-Z0-9"


lunitp :: Parser (Maybe ByteString)
lunitp = peekChar >>= \c -> case c of
                  Just '\n'   -> return Nothing
                  _           -> liftA Just $
                                  takeWhile ( inClass "a-zA-Z" )



-----------------------------------------------------------------------------
-- | Parser for __\/proc\/[pid]\/mountinfo__.
--
-- @
--  (openFile "/proc/1/mountinfo" ReadMode) >>=
--      \\h -> handleToInputStream h >>=
--          \\is -> parseFromStream mountInfop is
--
--  [MountInfo {_mountid = "14", _parentid = "18", ...]
-- @
mountInfop :: Parser [MountInfo]
mountInfop = sepBy mountInfoForRowp endOfLine


mountInfoForRowp :: Parser MountInfo
mountInfoForRowp = MountInfo
    <$> takeWhile isDigit <* skipJustSpacep
    <*> takeWhile isDigit <* skipJustSpacep
    <*> devMajMinNum <* skipJustSpacep
    <*> filePathp <* skipJustSpacep
    <*> filePathp <* skipJustSpacep
    <*> mountOptionsp <* skipJustSpacep
    <*> parseOptionalIfExists <* skipJustSpacep
    <*> fsTypep <* skipJustSpacep
    <*> mntSrcp <* skipJustSpacep
    <*> superOptionsp


devMajMinNum :: Parser (ByteString, ByteString)
devMajMinNum = (,) <$> (parseDigits <* char ':') <*> parseDigits


parseDigits :: Parser ByteString
parseDigits = takeWhile isDigit


parseOptionalIfExists :: Parser ( Maybe [(ByteString, ByteString)] )
parseOptionalIfExists = peekChar >>= (\c -> case c of
              Just '-' -> return Nothing
              _        -> liftA Just $  manyTill ( optionalFieldp <* skipJustSpacep )
                            $ char '-' )


optionalFieldp :: Parser (ByteString, ByteString)
optionalFieldp = let tagVal = takeWhile ( notInClass ": " ) in
            (,) <$> ( tagVal  <* char ':' ) <*> tagVal


mountOptionsp :: Parser [ByteString]
mountOptionsp = sepBy ( takeWhile $ notInClass ", " ) $ char ','


fsTypep :: Parser ByteString
fsTypep = takeWhile $ notInClass " "


mntSrcp :: Parser ByteString
mntSrcp = fsTypep


superOptionsp :: Parser [ByteString]
superOptionsp = sepBy ( takeTill $ inClass ",\n " ) $ char ','



-----------------------------------------------------------------------------
-- * Helper functions


-- | Parse the characters a-z A-Z 0-9 ( ) _ until a ":" is reached
idp :: Parser ByteString
idp = takeWhile ( inClass "a-zA-z0-9()_" ) <* (skipMany $ char ' ') <*  char ':'


-- | Parse kB
unitp :: Parser (Maybe ByteString)
unitp = option Nothing (string "kB" >>= \p -> return $ Just p)


doublep :: Parser ByteString
doublep = takeWhile ( inClass "0-9." ) <* space


-- | Parse hexadecimal
hdp :: Parser ByteString
hdp = takeWhile $ inClass "0-9a-f"


-- | Common parser for file paths
filePathp :: Parser ByteString
filePathp = takeWhile $ inClass "-a-zA-Z0-9.()[]_/,"

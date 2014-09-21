{-# LANGUAGE OverloadedStrings #-}
{-|
    Module          : Linux.Parser.Internal.Proc
    Descripition    : Parsers for the /proc vfs.

    The examples below use the __io-streams__ library.
-}

module Linux.Parser.Internal.Proc (
        -- * Data Types
        -- ** MappedMemory : \/proc\/[pid]\/maps
        MappedMemory (),
        mmAddress,
        mmPerms,
        mmOffset,
        mmDev,
        mmInode,
        mmPathname,

        -- ** Limits : \/proc\/[pid]\/limits
        Limit (),
        limitName,
        softLimit,
        hardLimit,
        unitOfLimit,

        -- ** Statm : \/proc\/[pid]\/statm
        Statm (),
        statmSize,
        statmResident,
        statmShare,
        statmText,
        statmLib,
        statmData,
        statmDt,

        -- ** MountInfo : \/proc\/[pid]\/mountinfo
        MountInfo (),
        miMountId,
        miParentId,
        miDevMajMinNum,
        miRoot,
        miMountPoint,
        miMountOptions,
        miOptionalFields,
        miFsType,
        miMountSource,
        miSuperOptions,


        -- * Parsers
        meminfop,
        loadavgp,
        uptimep,
        commp,
        iop,
        mapsp,
        environp,
        procstatp,
        statmp,
        numamapsp,
        limitsp,
        mountInfop
    ) where

import Control.Applicative hiding (empty)
import Control.Monad.Cont
import qualified Data.ByteString.Char8 as BC
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import Data.Maybe
import Data.ByteString hiding (takeWhile, count, foldl)

import Prelude hiding (takeWhile)

-------------------------------------------------------------------------------
-- | Data type for __\/proc\/[pid]\/maps__.
data MappedMemory = MM {

        _address :: (ByteString, ByteString),
        -- ^ Memory address in the form (start-address, end-address)

        _perms   :: ByteString,
        _offset  :: ByteString,

        _dev     :: (ByteString, ByteString),
        -- ^ Device number in the form (Major num, Minor num)

        _inode   :: ByteString,
        _pathname:: Maybe ByteString
    } deriving (Eq, Show)


mmAddress    = _address
mmPerms     = _perms
mmOffset    = _offset
mmDev       = _dev
mmInode     = _inode
mmPathname  = _pathname


-- | Data type for __\/proc\/[pid]\/limits__.
data Limit = Limit {
        _limit  :: [ByteString],
        _slimit :: ByteString,
        _hlimit :: ByteString,
        _unit   :: Maybe ByteString
    } deriving (Eq, Show)


limitName   = _limit
softLimit   = _slimit
hardLimit   = _hlimit
unitOfLimit = _unit


-- | Data type for __\/proc\/[pid]\/statm__.
data Statm = Statm {
        _size       :: ByteString,
        _resident   :: ByteString,
        _share      :: ByteString,
        _text       :: ByteString,
        _lib        :: ByteString,
        _data       :: ByteString,
        _dt         :: ByteString
    } deriving (Eq, Show)


statmSize       = _size
statmResident   = _resident
statmShare      = _share
statmText       = _text
statmLib        = _lib
statmData       = _data
statmDt         = _dt


-- Data type for __\/proc\/[pid]\/mountinfo__.
data MountInfo = MountInfo {
        _mountid        :: ByteString,
        _parentid       :: ByteString,
        _devmajmin      :: (ByteString, ByteString),
        _root           :: ByteString,
        _mountpoint     :: ByteString,
        _mountopts      :: [ByteString],
        _optionalfields :: Maybe [(ByteString, ByteString)],
        _fstype         :: ByteString,
        _mountsource    :: ByteString,
        _superoptions   :: [ByteString]
    } deriving (Eq, Show)


miMountId       = _mountid
miParentId      = _parentid
miDevMajMinNum  = _devmajmin
miRoot          = _root
miMountPoint    = _mountpoint
miMountOptions  = _mountopts
miOptionalFields= _optionalfields
miFsType        = _fstype
miMountSource   = _mountsource
miSuperOptions  = _superoptions


-------------------------------------------------------------------------------
-- | Parser for __\/proc\/meminfo__.
--
-- @
--  openFile "\/proc\/meminfo" ReadMode >>=
--      \h -> handleToInputStream h >>=
--          \is -> parseFromStream  meminfop is
--
-- [("MemTotal","4052076",Just "kB"),("MemFree","3450628",Just "kB"), ...]
-- @
meminfop :: Parser [(ByteString, ByteString, Maybe ByteString)]
meminfop = manyTill ((,,)
    <$> idp
    <*> ( skipspacep *> intp <* skipspacep )
    <*> unitp <* skipMany space) endOfInput



-------------------------------------------------------------------------------
-- | Parser for __\/proc\/[pid]\/stat__.
--
-- @
--  openFile "\/proc\/1\/stat" ReadMode >>=
--      \h -> handleToInputStream h >>=
--          \is -> parseFromStream procstatp is
--
--  ["1","(systemd)",\"S\","0","1", ...]
-- @
procstatp :: Parser [ByteString]
procstatp = manyTill psval endOfInput


psval ::  Parser ByteString
psval = ( takeWhile ( inClass "a-zA-z0-9()-" ) <* space )



------------------------------------------------------------------------------
-- | Parser for __\/proc\/[pid]\/statm__.
--
-- @
--  openFile "/proc/1/statm" ReadMode >>=
--      \h -> handleToInputStream h >>=
--          \is -> parseFromStream  (statmp) is
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
        parseVal = ( takeWhile isDigit ) <* skipspacep



------------------------------------------------------------------------------
-- | Parser for __\/proc\/loadavg__. Only parses the 1, 5 and 15 minute load
-- average, discards the fourth and fifth fields (kernel scheduling entities
-- and latest PID assigned).
--
-- @
--  openFile "\/proc\/loadavg" ReadMode >>=
--      \h -> handleToInputStream h >>=
--          \is -> parseFromStream loadavgp is
--
--  ("0.00","0.01","0.05")
-- @
loadavgp :: Parser (ByteString, ByteString, ByteString)
loadavgp = (,,) <$> doublep <*> doublep <*> doublep



------------------------------------------------------------------------------
-- | Parser for __\/proc\/uptime__. The first field is the uptime of the system
-- (seconds), the second is the amount of time spent in the idle process
-- (seconds).
--
-- @
--  openFile "\/proc\/uptime" ReadMode >>=
--      \h -> handleToInputStream h >>=
--          \is -> parseFromStream uptimep is
--
--  ("13048.12","78085.17")
-- @
uptimep :: Parser (ByteString, ByteString)
uptimep = (,) <$> doublep <*> doublep



------------------------------------------------------------------------------
-- | Parser for __\/proc\/[pid]\/comm__.
--
-- @
--  openFile "\/proc\/1\/comm" ReadMode >>=
--      \h -> handleToInputStream h >>=
--          \is -> parseFromStream (commp) is
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
--      \h -> handleToInputStream h >>=
--          \is -> parseFromStream (iop) is
--
--  [("rchar","12983399"),("wchar","14957379"), ...]
--
-- @
iop :: Parser [(ByteString, ByteString)]
iop = manyTill ((,) <$> idp <*> ( skipspacep *> intp <* skipMany space )  ) endOfInput



-------------------------------------------------------------------------------
-- | Parser for __\/proc\/[pid]\/maps__.
--
-- @
--  openFile "\/proc\/1\/maps" ReadMode >>=
--      \h -> handleToInputStream h >>=
--          \is -> parseFromStream mapsp is
--
--  [MM {_address = ("7f9c51069000","7f9c5107e000"), _perms = "r-xp", ...]
-- @
mapsp :: Parser [MappedMemory]
mapsp = manyTill ( mapsrowp <* endOfLine ) endOfInput



-- | Parse a row of \/proc\/[pid]\/maps
mapsrowp :: Parser MappedMemory
mapsrowp = MM
    <$> addressp <* skipspacep
    <*> permp <* skipspacep
    <*> hdp <* skipspacep
    <*> devicep <* skipspacep
    <*> intp <* skipspacep
    <*> pathnamep
    where
        addressp :: Parser (ByteString, ByteString)
        addressp = (,) <$> ( hdp <* char '-' ) <*> hdp

        permp :: Parser ByteString
        permp = takeWhile $ inClass "-rwxp"

        devicep :: Parser (ByteString, ByteString)
        devicep = (,) <$> ( intp <* char ':' ) <*> intp

        pathnamep :: Parser (Maybe ByteString)
        pathnamep = peekChar >>= \c -> case c of
                Just '\n'   -> return Nothing
                _           -> liftA Just $
                                takeWhile ( inClass "a-zA-Z0-9:/.[]-" )



-----------------------------------------------------------------------------
-- | Parser for __\/proc\/[pid]\/environ__.
--
-- @
--  openFile "/proc/373/environ" ReadMode >>=
--      \h -> handleToInputStream h >>=
--          \is -> parseFromStream environp is
--
--  [("LANG","en_IE.UTF-8"),("PATH","/usr/local/sbin"), ...]
-- @

environp :: Parser [(ByteString, ByteString)]
environp = sepBy environrowp $ char '\NUL'

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
--      \h -> handleToInputStream h >>=
--          \is -> parseFromStream numamapsp is
--
-- [("7f9c51069000","default",["file=/usr/lib/libz.so.1.2.8", ...]
-- @
numamapsp :: Parser [(ByteString, ByteString, [ByteString])]
numamapsp = manyTill ( (,,)
    <$> ( hdp <* skipspacep )
    <*> ( takeWhile ( inClass "a-zA-Z" ) <* skipspacep )
    <*> ( sepBy ( takeWhile $ inClass "-a-zA-Z0-9=/." ) $ char ' ' )
    <*  endOfLine ) endOfInput



-----------------------------------------------------------------------------
-- | Parser for \/proc\/[pid]\/limits.
--
-- @
--  openFile "/proc/1/limits" ReadMode >>=
--      \h -> handleToInputStream h >>=
--          \is -> parseFromStream limitsp is
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
    <$> limitnamep  <* skipspacep
    <*> shlimitp    <* skipspacep
    <*> shlimitp    <* skipspacep
    <*> lunitp      <* skipspacep


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
-- Parser for __\/proc\/[pid]\/mountinfo__.
--
-- @
--  (openFile "/proc/1/mountinfo" ReadMode) >>= 
--      \h -> handleToInputStream h >>= 
--          \is -> parseFromStream mountInfop is
--  
--  [MountInfo {_mountid = "14", _parentid = "18", ...]
-- @
mountInfop :: Parser [MountInfo]
mountInfop = sepBy mountInfoForRowp endOfLine


mountInfoForRowp :: Parser MountInfo
mountInfoForRowp = MountInfo
    <$> takeWhile isDigit <* skipspacep
    <*> takeWhile isDigit <* skipspacep
    <*> devMajMinNum <* skipspacep
    <*> filePathp <* skipspacep
    <*> filePathp <* skipspacep
    <*> mountOptionsp <* skipspacep
    <*> parseOptionalIfExists <* skipspacep
    <*> fsTypep <* skipspacep
    <*> mntSrcp <* skipspacep
    <*> superOptionsp


devMajMinNum :: Parser (ByteString, ByteString)
devMajMinNum = (,) <$> (parseDigits <* char ':') <*> parseDigits


parseDigits :: Parser ByteString
parseDigits = takeWhile isDigit


parseOptionalIfExists :: Parser ( Maybe [(ByteString, ByteString)] )
parseOptionalIfExists = peekChar >>= (\c -> case c of
              Just '-' -> return Nothing
              _        -> liftA Just $  manyTill ( optionalFieldp <* skipspacep )
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

-- | Skip only zero or many " "
skipspacep :: Parser ()
skipspacep =  (skipMany $ char ' ')


-- | Parse the characters a-z A-Z 0-9 ( ) _ until a ":" is reached
idp :: Parser ByteString
idp = takeWhile ( inClass "a-zA-z0-9()_" ) <* (skipMany $ char ' ') <*  char ':'


-- | Parse and integer
intp :: Parser ByteString
intp = takeWhile $ inClass "0-9"


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

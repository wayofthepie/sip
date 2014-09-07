
-- * Example usage
--     liftM (parse lsofp "") $ readProcess "lsof" ["-F","cgpRu"] ""

module Linux.Parser.Internal.Lsof where

import Control.Applicative hiding (many,(<|>))
import Data.Graph.Inductive
import System.Process
import Text.Parsec
import Text.Parsec.String

{-
data LsofCST = LsofCST {
        _accessMode         :: String,  -- a    file access mode
        _procCmdName        :: String,  -- c    process command name
        --_fStructShareCount  :: String,  -- C    file structure share count
        _fDecCharCode       :: String,  -- d    file's device character code
        _fdevNum            :: String,  -- D    file's major/minor device number (0x<hexadecimal>)
        _fDescriptor        :: String,  -- f    file descriptor
        _fStructAddr        :: String,  -- F    file structure address (0x<hexadecimal>)
        _fFlags             :: String,  -- G    file flaGs (0x<hexadecimal>; names if +fg follows)
        _procGroupId        :: Int,     -- g    process group ID
        _fInodeNum          :: String,  -- i    file's inode number
        _taskId             :: String,  -- K    tasK ID
        _linkCount          :: String,  -- k    link count
        _fLockStatus        :: String,  -- l    file's lock status
        _procLoginName      :: String,  -- L    process login name
        _m                  :: String,  -- m    marker between repeated output
        _fNameCommentIAddr  :: String,  -- n    file name, comment, Internet address
        _nodeId             :: String,  -- N    node identifier (ox<hexadecimal>)
        _fOffset            :: String,  -- o    file's offset (decimal)
        _pid                :: Int,     -- p    process ID (always selected)
        _protocolName       :: String,  -- P    protocol name
        --_rawDevNum          :: String,  -- r    raw device number (0x<hexadecimal>)
        _ppid               :: String,  -- R    parent process ID
        _fSize              :: String,  -- s    file's size (decimal)
        _fStreamId          :: String,  -- S    file's stream identification
        _fType              :: String,  -- t    file's type
        _tcpTpiInfo         :: String,  -- T    TCP/TPI information, identified by prefixes
        _procUserId         :: String   -- u    process user ID
    } deriving (Eq, Show)
-}

{- data NodeLabel = FDType String | File String deriving (Eq, Show)
data PIDInfo = PIDInfo Int deriving (Eq, Show)

fdTypeLbl :: String -> NodeLabel
fdTypeLbl n = FDType n


fileLbl :: String -> NodeLabel
fileLbl f = File f
-}

data PIDInfo = PIDInfo {
        _pid    :: Int,
        _gid    :: Int,
        _ppid   :: Int,
        _cmdname:: String,
        _uid    :: Int
    } deriving (Eq, Show)

data FileInfo = FileInfo {
        _fd         :: String,
        _inode      :: Int,
        _fileName   :: String
    } deriving (Eq, Show)


lsofp :: Parser [ ( PIDInfo, [FileInfo] ) ]
lsofp = manyTill ( (,) <$> pidInfop <*> many fileSetp ) eof


fileSetp :: Parser FileInfo
fileSetp = FileInfo
    <$> fdp <* newline 
    <*> option (-1) ( inodep <* newline ) 
    <*> fileNamep <* newline
    

pidInfop :: Parser PIDInfo
pidInfop = PIDInfo
        <$> pidp <* newline
        <*> procGroupIdp <* newline
        <*> ppidp <* newline
        <*> procCmdNamep <* newline
        <*> procUserIdp <* newline


procCmdNamep :: Parser String
procCmdNamep = char 'c' *> many ( alphaNum <|> oneOf "/:_-()." )


fdp :: Parser String
fdp = char 'f' *> many alphaNum


procGroupIdp :: Parser Int
procGroupIdp = char 'g' *> numStrToIntp


inodep :: Parser Int
inodep = char 'i' *> numStrToIntp


fileNamep :: Parser String
fileNamep = char 'n' *> many ( alphaNum <|> oneOf "/:_-().[]*:=>@" )


pidp :: Parser Int
pidp = char 'p' *> numStrToIntp


-- | ppidp : parses a ppid
ppidp :: Parser Int
ppidp = char 'R' *> numStrToIntp


procUserIdp :: Parser Int
procUserIdp = char 'u' *> numStrToIntp

-------------------------------------------------------------------------------
-- | Helper functions
-------------------------------------------------------------------------------
-- | numStrToIntp : parses a string of digits
numStrToIntp :: Parser Int
numStrToIntp = liftA read $ many1 digit


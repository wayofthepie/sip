
module Linux.Parser.Lsof {-(
        PIDInfo (),
        FileInfo (),

        lsofp,

        pid,
        gid,
        ppid,
        cmdname,
        uid,

        fd,
        inode,
        fname
    ) -}
 where


import Control.Applicative hiding ((<|>))
import Data.Graph.Inductive
import Data.Attoparsec.Text
--import Text.Parsec
--import Text.Parsec.String

import Linux.Parser.Internal.Lsof


type LsofCST = [ ( PIDInfo, [FileInfo] ) ]

data PIDInfo = PIDInfo {
        _pid    :: Int,     -- ^ Process ID
        _gid    :: Int,     -- ^ Process group ID
        _ppid   :: Int,     -- ^ Parent process ID
        _cmdname:: String,  -- ^ Process command name
        _uid    :: Int      -- ^ Process user id
    } deriving (Eq, Ord, Show)


pid :: PIDInfo -> Int
pid = _pid


gid :: PIDInfo -> Int
gid = _gid


ppid :: PIDInfo -> Int
ppid = _ppid


cmdname :: PIDInfo -> String
cmdname = _cmdname


uid :: PIDInfo -> Int
uid = _uid



data FileInfo = FileInfo {
        _fd         :: String,  -- ^ File descriptor
        _inode      :: Int,     -- ^ inode number
        _fname      :: String   -- ^ File name corresponding to inode number
    } deriving (Eq, Ord, Show)


fd :: FileInfo -> String
fd = _fd


inode :: FileInfo -> Int
inode = _inode


fname :: FileInfo -> String
fname = _fname


--------------------------------------------------------------------------------
-- High level parsers
--------------------------------------------------------------------------------

lsofp :: Parser LsofCST
lsofp = manyTill ( (,) <$> pidInfop <*> many fileSetp ) endOfInput


fileSetp :: Parser FileInfo
fileSetp = FileInfo
    <$> fdp <* endOfLine
    <*> option (-1) ( inodep <* endOfLine )
    <*> fileNamep <* endOfLine


pidInfop :: Parser PIDInfo
pidInfop = PIDInfo
        <$> pidp <* endOfLine
        <*> procGroupIdp <* endOfLine
        <*> ppidp <* endOfLine
        <*> procCmdNamep <* endOfLine
        <*> procUserIdp <* endOfLine


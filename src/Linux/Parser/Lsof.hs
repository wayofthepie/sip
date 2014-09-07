module Linux.Parser.Lsof where

import Control.Applicative hiding (many,(<|>))
import Data.Graph.Inductive
import Text.Parsec
import Text.Parsec.String

import Linux.Parser.Internal.Lsof


type LsofCST = [ ( PIDInfo, [FileInfo] ) ] 

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


lsofp :: Parser LsofCST 
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


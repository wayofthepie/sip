{-|
    Module          : Linux.Parser.Internal.Lsof
    Descripition    : Parsers for lsof output

    Currently the top-level parser __lsofp__ and the two parsers it calls
    (__fileSetp__ and __pidInfop__) parse the output of __lsof -F cfgpRuni__
    and expect this output to be in the following order:

    (1) pid
    (2) gid
    (3) ppid
    (4) cmdname
    (5) uid
    (6) multiple rows of file information:
        
        (1) fd
        (2) inode (may not be present)
        (3) fileName

    Example command to run the parser:
       
       @ 
        liftM (parse lsofp "") $ readProcess "lsof" ["-n", "-F","cfgpRuni"] "" 
       @
-}
module Linux.Parser.Internal.Lsof where

import Control.Applicative hiding (many,(<|>))
import Data.Graph.Inductive
import System.Process
import Text.Parsec
import Text.Parsec.String


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


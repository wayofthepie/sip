{-# LANGUAGE OverloadedStrings #-}
{-|
    Module          : Linux.Parser.Internal.Proc
    Descripition    : Parsers for the /proc vfs.

    The examples below use the __io-streams__ library.
-}

module Linux.Parser.Internal.Proc ( 
        meminfop,
        procstatp
    ) where

import Control.Applicative
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import Data.Maybe
import Data.ByteString hiding (takeWhile)

import Prelude hiding (takeWhile)

-------------------------------------------------------------------------------
-- | Parser for \/proc\/meminfo.
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
    <*> ( skipspacep *> valp <* skipspacep ) 
    <*> unitp <* skipMany space) endOfInput


-- | Internal parsers for meminfo

skipspacep :: Parser ()
skipspacep =  (skipMany $ char ' ')


idp :: Parser ByteString
idp = takeWhile ( inClass "a-zA-z0-9()_" ) <* (skipMany $ char ' ') <*  char ':'


valp :: Parser ByteString
valp = takeWhile $ inClass "0-9" 
    

unitp :: Parser (Maybe ByteString)
unitp = option Nothing (string "kB" >>= \p -> return $ Just p)


-------------------------------------------------------------------------------
-- | Parser for \/proc\/[pid]\/stat.
--
-- @
--  openFile "/proc/1/stat" ReadMode >>= 
--      \h -> handleToInputStream h >>= 
--          \is -> parseFromStream procstatp is
--
--  ["1","(systemd)",\"S\","0","1", ...]
-- @
procstatp :: Parser [ByteString]
procstatp = manyTill (takeWhile ( inClass "a-zA-z0-9()-" ) <* space) endOfInput



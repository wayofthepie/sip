{-# LANGUAGE OverloadedStrings #-}
{-|
    Module          : Linux.Parser.Internal.Proc
    Descripition    : Parsers for the /proc vfs.

    The examples below use the __io-streams__ library.
-}

module Linux.Parser.Internal.Proc ( 
        meminfop,
        procstatp,
        loadavgp,
        uptimep,
        commp
    ) where

import Control.Applicative
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import Data.Maybe
import Data.ByteString hiding (takeWhile)

import Prelude hiding (takeWhile)

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
procstatp = manyTill (takeWhile ( inClass "a-zA-z0-9()-" ) <* space) endOfInput



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
--  openFile "/proc/uptime" ReadMode >>= 
--      \h -> handleToInputStream h >>= 
--          \is -> parseFromStream uptimep is
--
--  ("13048.12","78085.17")
-- @
uptimep :: Parser (ByteString, ByteString)
uptimep = (,) <$> doublep <*> doublep 



------------------------------------------------------------------------------
---- | Parser for __\/proc\/comm__.
commp :: Parser ByteString
commp = takeWhile ( inClass "a-zA-Z0-9:/" )



------------------------------------------------------------------------------
-- | Helper functions

doublep :: Parser ByteString
doublep = takeWhile ( inClass "0-9." ) <* space


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
        commp,
        iop,
        mapsrowp
    ) where

import Control.Applicative
import qualified Data.ByteString.Char8 as BC
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import Data.Maybe
import Data.ByteString hiding (takeWhile, count)

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
    <*> ( skipspacep *> intp <* skipspacep ) 
    <*> unitp <* skipMany space) endOfInput


-- | Internal parsers for meminfo



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



-------------------------------------------------------------------------------
---- | Parser for __\/proc\/comm__.
iop :: Parser [(ByteString, ByteString)]
iop = manyTill ((,) <$> idp <*> ( skipspacep *> intp <* skipMany space )  ) endOfInput
    


-------------------------------------------------------------------------------
---- | Parser for __\/proc\/comm__.
data MappedMemory = MM {
       _address :: ByteString,
       _perms   :: ByteString, 
       _offset  :: ByteString,
       _dev     :: ByteString,
       _inode   :: ByteString,
       _pathname:: ByteString        
    } deriving (Eq, Show)

{-mapsp :: Parser [MappedMemory]
mapsp = manyTill 
    <$> ( MM <$> ( hexadecimal <* char '-' *> hexadecimal) <* skipspacep
    <*> (liftA pack $ count 4 $ choice [char '-', char 'r', char 'w', char 'x', char 'p' ] ))
    endOfInputi-}

mapsrowp :: Parser MappedMemory
mapsrowp = MM 
    <$> ( hdp <* char '-' *> hdp) <* skipspacep
    <*> ( takeWhile $ inClass "-rwxp" ) <* skipspacep
    <*> intp <* skipspacep
    <*> ( (intp >>= \i -> char ':' >>= \c -> return $ BC.snoc i c ) >>= 
            \bs -> intp >>= \i -> return $ BC.append bs i ) <* skipspacep 
    <*> intp <* skipspacep
    <*> takeWhile ( inClass "a-zA-Z0-9:/" )

hdp = takeWhile $ inClass "0-9a-f"
--( do i <- intp; c <- char ':'; return $ BC.snoc i c )

-----------------------------------------------------------------------------
-- | Helper functions

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



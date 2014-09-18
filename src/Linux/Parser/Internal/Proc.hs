{-# LANGUAGE OverloadedStrings #-}
{-|
    Module          : Linux.Parser.Internal.Proc
    Descripition    : Parsers for the /proc vfs
-}

module Linux.Parser.Internal.Proc ( 
        meminfop,
        procstatp
    ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import Data.Maybe
import Data.ByteString hiding (takeWhile)
import System.IO
import System.IO.Streams.Attoparsec

import Prelude hiding (takeWhile)

-------------------------------------------------------------------------------
-- | Parser for \/proc\/meminfo. Example usage (this example makes use of the 
-- __io-streams__ library):
--
-- @
-- openFile "\/proc\/meminfo" ReadMode >>= 
--     \h -> handleToInputStream h >>= 
--         \is -> parseFromStream  meminfop is
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
procstatp :: Parser [ByteString]
procstatp = manyTill (takeWhile ( inClass "a-zA-z0-9()-" ) <* space) endOfInput



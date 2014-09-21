{-# LANGUAGE OverloadedStrings #-}
{-|
    Module          : Linux.Parser.Internal.ProcNet
    Descripition    : Parsers for the \/proc\/net\/ and \/proc\/[pid]\/net\/.

    The examples below use the __io-streams__ library.
-}

module Linux.Parser.Internal.ProcNet where

import Control.Applicative hiding (empty)
import Data.ByteString.Char8 
--import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import Data.Maybe
import Data.ByteString hiding (takeWhile, count, foldl)

import Prelude hiding (takeWhile)

import Linux.Parser.Internal.Common

-------------------------------------------------------------------------------
-- * __\/proc\/net\/dev__

data NetDeviceInfo = NetDeviceInfo {
        _deviceName     :: ByteString,
        _rcvBytes       :: ByteString,
        _rcvPackets     :: ByteString,
        _rcvErrors      :: ByteString,
        _rcvDrop        :: ByteString,
        _rcvFifo        :: ByteString,
        _rcvFrame       :: ByteString,
        _rcvCompressed  :: ByteString,
        _rcvMulticast   :: ByteString,
        _transBytes     :: ByteString,
        _transPackets   :: ByteString,
        _transErrors    :: ByteString,
        _transDrop      :: ByteString,
        _transFifo      :: ByteString,
        _transColls     :: ByteString,
        _transCarrier   :: ByteString,
        _transCompressed:: ByteString
    } deriving (Eq, Show)

-- | Parser for __\/proc\/net\/dev__.
netDevp :: Parser [NetDeviceInfo]
netDevp = skipLine *> skipLine *> manyTill ( netDevRowp <* endOfLine ) endOfInput


netDevRowp :: Parser NetDeviceInfo
netDevRowp = NetDeviceInfo 
    <$> ( skipJustSpacep *> deviceNamep ) <* skipJustSpacep
    <*> intp <* skipJustSpacep 
    <*> intp <* skipJustSpacep     
    <*> intp <* skipJustSpacep
    <*> intp <* skipJustSpacep
    <*> intp <* skipJustSpacep
    <*> intp <* skipJustSpacep
    <*> intp <* skipJustSpacep
    <*> intp <* skipJustSpacep
    <*> intp <* skipJustSpacep
    <*> intp <* skipJustSpacep
    <*> intp <* skipJustSpacep
    <*> intp <* skipJustSpacep
    <*> intp <* skipJustSpacep
    <*> intp <* skipJustSpacep
    <*> intp <* skipJustSpacep
    <*> intp 

deviceNamep :: Parser ByteString
deviceNamep = takeTill ( inClass ":" ) <* char ':'


skipLine :: Parser ()
skipLine = skipWhile ( not . isEndOfLine ) >> endOfLine 
    where isEndOfLine c = if c == '\n' then True else False

{-# LANGUAGE OverloadedStrings #-}
{-|
    Module          : Linux.Parser.Internal.ProcNet
    Descripition    : Parsers for the \/proc\/net\/ and \/proc\/[pid]\/net\/.
-}

module Linux.Parser.Internal.ProcNet where

import Control.Applicative hiding (empty)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString hiding (takeWhile, count, foldl)

import Prelude hiding (takeWhile)

import Linux.Parser.Internal.Common

-------------------------------------------------------------------------------
-- * __\/proc\/net\/dev__

data NetDeviceInfo = NetDeviceInfo
    { _deviceName     :: ByteString
    , _rcvBytes       :: ByteString
    , _rcvPackets     :: ByteString
    , _rcvErrors      :: ByteString
    , _rcvDrop        :: ByteString
    , _rcvFifo        :: ByteString
    , _rcvFrame       :: ByteString
    , _rcvCompressed  :: ByteString
    , _rcvMulticast   :: ByteString
    , _transBytes     :: ByteString
    , _transPackets   :: ByteString
    , _transErrors    :: ByteString
    , _transDrop      :: ByteString
    , _transFifo      :: ByteString
    , _transColls     :: ByteString
    , _transCarrier   :: ByteString
    , _transCompressed:: ByteString
    } deriving (Eq, Show)

-- | Parser for __\/proc\/net\/dev__.
--
-- Example
--
-- @
--   import Data.Attoparsec.ByteString
--  import qualified Data.ByteString as BS
--
--  BS.readFile "\/proc\/net\/dev" >>= \\bs -> return $ parseOnly netDevp bs
--
-- @
netDevp :: Parser [NetDeviceInfo]
netDevp = skipLinep *> skipLinep *> manyTill ( netDevRowp ) endOfInput


netDevRowp :: Parser NetDeviceInfo
netDevRowp = builder
    <$> ( skipJustSpacep *> deviceNamep ) <* skipJustSpacep
    <*> manyTill (intp <* skipJustSpacep) endOfLine
    where
        -- Not sure of any other way to do this ...
        builder :: ByteString -> [ByteString] -> NetDeviceInfo
        builder a (b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:q:[]) =
            NetDeviceInfo a b c d e f g h i j k l m n o p q


deviceNamep :: Parser ByteString
deviceNamep = takeTill ( inClass ":" ) <* char ':'


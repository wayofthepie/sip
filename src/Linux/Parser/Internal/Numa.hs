

module Linux.Parser.Internal.Numa where

import Control.Applicative hiding (empty)
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import Data.ByteString hiding (takeWhile, count, foldl)

import Prelude hiding (takeWhile)

import Linux.Parser.Internal.Common


-----------------------------------------------------------------------------
-- | Parser for \/proc\/[pid]\/numa_maps. Generates a list of 3-tuples :
-- (start addr of mem range, mem policy for range, extra info on pages in
-- range).
--
-- @
--  openFile "/proc/1/numa_maps" ReadMode >>=
--      \\h -> handleToInputStream h >>=
--          \\is -> parseFromStream numamapsp is
--
-- [("7f9c51069000","default",["file=/usr/lib/libz.so.1.2.8", ...]
-- @


data NumaMap = NumaMap
    { memStart :: ByteString
    , memPolicy:: ByteString
    , mappingDetails :: ByteString
    } deriving (Eq, Show)

numamapsp :: Parser [(ByteString, ByteString, [ByteString])]
numamapsp = manyTill ( (,,)
    <$> ( hdp <* skipJustSpacep )
    <*> ( takeWhile ( inClass "a-zA-Z" ) <* skipJustSpacep )
    <*> ( sepBy ( takeWhile $ inClass "-a-zA-Z0-9=/." ) $ char ' ' )
    <*  endOfLine ) endOfInput



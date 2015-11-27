

module Linux.Parser.Internal.Numa where

import Control.Applicative hiding (empty)
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import Data.ByteString hiding (takeWhile, count, foldl)
import qualified Data.Map as Map
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
    { memStart       :: ByteString
    , memPolicy      :: ByteString
    , mappingDetails :: Map.Map ByteString ByteString
    } deriving (Eq, Show)

numamapsp :: Parser [NumaMap]
numamapsp = sepBy ( NumaMap
    <$> ( hdp <* skipJustSpacep )
    <*> ( takeWhile ( inClass "a-zA-Z" ) <* skipJustSpacep )
    <*> option Map.empty parseMappingDetails ) endOfLine

parseMappingDetails :: Parser ( Map.Map ByteString ByteString )
parseMappingDetails = Map.fromList <$> ( sepBy parseMappingDetail $ char ' ' )
  where
    parseMappingDetail = (,)
        <$> ( ( takeTill $ inClass "=" ) <* char '=' )
        <*> ( takeTill $ inClass " \n" )






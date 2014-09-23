
{-|
    Module          : Linux.Parser.Internal.ProcNet
    Descripition    : Parsers for the \/proc\/net\/ and \/proc\/[pid]\/net\/.

    The examples below use the __io-streams__ library.
-}

module Linux.Parser.Internal.Common where

import Control.Applicative hiding (empty)
import qualified Data.ByteString.Char8 as BC
import Data.Attoparsec.ByteString.Char8
import Data.ByteString hiding (takeWhile, count, foldl)

import Prelude hiding (takeWhile)

-- | Skip only zero or many " "
skipJustSpacep :: Parser ()
skipJustSpacep = skipMany $ char ' '


-- | Parse and integer
intp :: Parser ByteString
intp = takeWhile $ inClass "0-9"

-- | Parse a line, throw away the result
skipLinep :: Parser ()
skipLinep = skipWhile ( not . isEndOfLine ) >> endOfLine 
    where isEndOfLine c = if c == '\n' then True else False

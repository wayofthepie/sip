{-|
    Module          : Linux.Parser.Internal.Lsof
    Descripition    : Parsers for lsof output

    Currently the top-level parser __lsofp__ and the two parsers it calls
    (__fileSetp__ and __pidInfop__) parse the output of __lsof -F cfgpRuni__
    and expect this output to be in the order __pid__, __gid__, __ppid__, 
    __cmdname__ and __uid__ proceeded by a repeating list of the following info: 
    __fd__, __inode__ (which may not be present) and __fileName__.

    Example command to run the parser:
       
       @ 
       liftM (parse lsofp "") $ readProcess "lsof" ["-n", "-F","cfgpRuni"] ""
       @
-}
module Linux.Parser.Internal.Lsof where

import Control.Applicative hiding (many,(<|>))
import Text.Parsec
import Text.Parsec.String


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


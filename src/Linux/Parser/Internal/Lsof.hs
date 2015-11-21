{-|
    Module          : Linux.Parser.Internal.Lsof
    Descripition    : Parsers for lsof output

    Example command to run the parser:

       @
       liftM (parse lsofp "") $ readProcess "lsof" ["-n", "-F","cfgpRuni"] ""
       @
-}
module Linux.Parser.Internal.Lsof where

import Control.Applicative
import Data.Attoparsec.Text


procCmdNamep :: Parser String
procCmdNamep = char 'c' *> many ( alphaNum <|> satisfy (flip elem "/:_-().") )


fdp :: Parser String
fdp = char 'f' *> many alphaNum


procGroupIdp :: Parser Int
procGroupIdp = char 'g' *> numStrToIntp


inodep :: Parser Int
inodep = char 'i' *> numStrToIntp


fileNamep :: Parser String
fileNamep = char 'n' *> many ( alphaNum <|> satisfy (flip elem "/:_-().[]*:=>@") )


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

alphaNum :: Parser Char
alphaNum = letter <|> digit

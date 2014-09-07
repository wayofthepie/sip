
module Linux.Graph.SystemInfo where

import Data.Graph.Inductive as GI

import Linux.Parser.Lsof


data NodeLabel = FDType String | File String deriving (Eq, Show)




{-g :: Gr [NodeLabel] [PIDInfo] 
g = GI.mkGraph [(1, fdTypeLbl "cwd"),(2, fileLbl "/"),(3, fileLbl "/")] [(1,2,"s")] -}



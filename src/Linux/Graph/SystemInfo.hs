
module Linux.Graph.SystemInfo where

import Control.Monad
import Data.Graph.Inductive 
import Data.Graph.Inductive.NodeMap 
import Data.Map as M hiding (map, foldl, empty)
import Linux.Parser.Lsof


data NodeLabel = PID PIDInfo | File FileInfo deriving (Eq, Ord, Show)


lsofToGraph :: [(PIDInfo, [FileInfo])] -> Gr NodeLabel String
lsofToGraph = lsofToGraph' empty 


lsofToGraph' :: Gr NodeLabel String -> LsofCST -> Gr NodeLabel String
lsofToGraph' g []   = g
lsofToGraph' g xs   = run_ empty $ mapM genNodeMapM xs

genNodeMapM :: DynGraph g =>  (PIDInfo, [FileInfo]) -> NodeMapM NodeLabel String g [()]
genNodeMapM (pidInfo, fileInfos) = 
    insMapNodeM (PID pidInfo) >> 
        mapM (\fileInfo ->  insMapNodeM (File fileInfo) >>= 
            (\_ -> insMapEdgeM (PID pidInfo, File fileInfo, fd fileInfo))) fileInfos
                

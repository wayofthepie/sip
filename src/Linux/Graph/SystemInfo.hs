
module Linux.Graph.SystemInfo (
        NodeLabel,
        lsofToGraph
    ) where

import Control.Monad
import Data.Graph.Inductive 
import Data.Graph.Inductive.NodeMap 
import Data.Map as M hiding (map, foldl, empty)
import Linux.Parser.Lsof


data NodeLabel = PID PIDInfo | File FileInfo deriving (Eq, Ord, Show)


-- | lsofToGraph l : the graph of the list l (PIDInfo and related FileInfo) constructed by 
--      mapping pidinfo nodes to fileinfo nodes with the filedescriptor type (e.g. mem, cwd) 
--      as edges
lsofToGraph :: [(PIDInfo, [FileInfo])] -> Gr NodeLabel String
lsofToGraph = lsofToGraph' empty 


-- | lsofToGraph' g l : auxiliary function for lsofToGraph 
lsofToGraph' :: Gr NodeLabel String -> [(PIDInfo, [FileInfo])]-> Gr NodeLabel String
lsofToGraph' g []   = g
lsofToGraph' g xs   = run_ empty $ mapM genNodeMapM xs


-- | genNodeMapM l : nodemap generate from the pidinfo/fileinfo list l
genNodeMapM :: DynGraph g =>  (PIDInfo, [FileInfo]) -> NodeMapM NodeLabel String g [()]
genNodeMapM (pidInfo, fileInfos) = 
    insMapNodeM (PID pidInfo) >> 
        mapM (\fileInfo ->  insMapNodeM (File fileInfo) >>= 
            (\_ -> insMapEdgeM (PID pidInfo, File fileInfo, fd fileInfo))) fileInfos
                

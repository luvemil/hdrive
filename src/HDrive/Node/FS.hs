module HDrive.Node.FS where

import Control.Lens
import Data.Foldable (Foldable (foldl'), foldlM)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import HDrive.Node.Types.DirNode (newDir)
import HDrive.Node.Types.FS (FSElem (..), FSMap)
import HDrive.Node.Types.FileNode (FileNode)
import System.FilePath (splitPath, (</>))
import System.FilePath.Lens

addToLast :: [FilePath] -> FilePath -> [FilePath]
addToLast acc cur = acc <> [fromMaybe "/" (acc ^? _last) </> cur]

computeDirHierarchy :: FilePath -> [FilePath]
computeDirHierarchy fp = foldl' addToLast [] (map ensureDirDoesntEndInSlash . splitPath $ "/" </> fp)

addDirDeep :: FSMap -> FilePath -> IO FSMap
addDirDeep cont fp = do
    let aggregated = computeDirHierarchy fp
        addIfNeeded :: FSMap -> FilePath -> IO FSMap
        addIfNeeded fsMap dirFp
            | M.member dirFp fsMap = pure fsMap
            | dirFp == "/" = case M.lookup "/" fsMap of
                Nothing -> pure $ M.insert "/" [] fsMap
                Just _ -> pure fsMap
            | otherwise =
                do
                    dirElem <- FSDir <$> newDir dirFp
                    pure $
                        fsMap
                            & M.update (\els -> Just (els <> [dirElem])) (dirFp ^. directory)
                            & M.insert dirFp []

    foldlM addIfNeeded cont aggregated

ensureDirDoesntEndInSlash :: FilePath -> FilePath
ensureDirDoesntEndInSlash "/" = "/"
ensureDirDoesntEndInSlash s = case s ^? _last of
    Nothing -> ""
    Just '/' -> s ^. directory
    Just _ -> s

insertFileNode :: FSMap -> FileNode -> IO FSMap
insertFileNode cont fn = do
    let filePath = ensureDirDoesntEndInSlash $ "/" </> fn ^. #path . #_S3FilePath . directory
    baseFS <-
        if M.member filePath cont
            then pure cont
            else addDirDeep cont filePath
    pure $ M.insertWith (<>) filePath [FSFile fn] baseFS

mapToFS :: [FileNode] -> IO FSMap
mapToFS fns = do
    let rootFS = M.singleton "/" []
    foldlM insertFileNode rootFS fns

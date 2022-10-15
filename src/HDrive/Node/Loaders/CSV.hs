module HDrive.Node.Loaders.CSV where

import Control.Lens.Operators
import Data.Maybe (mapMaybe)
import HDrive.Node.Types.FileNode
import System.FilePath.Lens (basename)
import Text.CSV
import qualified Text.Read as T

filesFromCSV :: CSV -> [FileNode]
filesFromCSV = mapMaybe getFileFromRecord

-- Removes the initial slash. Fixes a problem with JsonToPostgres.addFile, but maybe
-- it would be better to fix the problem over there
removeInitSlash :: String -> String
removeInitSlash ('/' : xs) = removeInitSlash xs
removeInitSlash xs = xs

parseFileType :: MonadFail m => String -> m FileType
parseFileType s = case s of
    "IMAGE" -> pure FileImage
    "VIDEO" -> pure FileVideo
    "OTHER" -> pure FileOther
    _ -> fail $ "Cannot parse file type: " <> s

getFileFromRecord :: Record -> Maybe FileNode
getFileFromRecord r
    | length r < 5 = Nothing
    | otherwise = case take 5 r of
        [uuidS, pathS, fileTypeS, sizeS, thumbS] -> do
            fileType <- parseFileType fileTypeS
            -- TODO: add monadic parsing and FromCSV typeclass
            let size = T.readMaybe @Int sizeS
                title = pathS ^. basename
                path = S3FilePath (removeInitSlash pathS)
                thumb = Just thumbS
                uuid = FileId uuidS -- the uuid must be used as a reference to get the file from s3
            pure FileNode{..}
        _ -> Nothing

loadCsvFromFile :: FilePath -> IO [FileNode]
loadCsvFromFile csvStore = do
    fCSV <- parseCSVFromFile csvStore
    case fCSV of
        Left pe -> fail . show $ pe
        Right csv -> pure $ filesFromCSV csv
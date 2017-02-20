module Externals (externals) where

import           Text.CSV
import           Crypto.Hash
import           Control.Monad
import           Traverse
import qualified Data.ByteString.Lazy as L
import           Data.List
import           System.Directory

-- in current directory
findExternalFiles :: IO [FilePath]
findExternalFiles =
    filter (isSuffixOf ".externals") <$> getDirectoryContents "."

type Url = String
type RepoDig = String
type FileDig = String

type ExternalContent = (FilePath, FileDig, RepoDig, Url, FilePath)
type External = [ExternalContent]

hashFile :: FilePath -> IO (Digest SHA512)
hashFile fp = hashlazy <$> L.readFile fp

readExternalFile :: IO External
readExternalFile = do
    files <- findExternalFiles
    case files of
        []             -> error "no external file found in current directory"
        _:_:_          -> error "multiple externals file found in current directory"
        [externalFile] -> either (error . show) toExternal <$> parseCSVFromFile externalFile
  where
    toExternal :: CSV -> External
    toExternal recs = map parseRecord recs

    parseRecord :: Record -> ExternalContent
    parseRecord (file:fileDigest:repoDigest:repoUrl:repoFile:[]) = (file, fileDigest, repoDigest, repoUrl, repoFile)
    parseRecord x                                                = error ("unknown line: " ++ show x)

writeExternalFile :: FilePath -> External -> IO ()
writeExternalFile fileName ext =
    writeFile fileName $ printCSV $ map toRecord ext
  where
    toRecord (file,fileDigest,repoDigest,repoUrl,repoFile) = [file, fileDigest, repoDigest, repoUrl, repoFile]

doScan extFile dir = do
    externalConfigs <- findExternalFiles
    unless (null externalConfigs) $ error "externals file existing"

    let fileCb acc file
            | isSuffixOf ".c" file || isSuffixOf ".h" file = do
                hsh <- hashFile file
                return $ (file, show hsh, "", "", "") : acc
            | otherwise =
                return acc
        dirCb acc _ = do
            return (True, acc)

    scanned <- dirTraverse dir fileCb dirCb []
    writeExternalFile extFile scanned
    
doInit = undefined

doImport = undefined

externals args =
    case args of
        "init":extfile:[]           -> doInit extfile
        "scan":extfile:dir:[]       -> doScan extfile dir
        "import":destination:url:[] -> doImport destination url
        "check":[]                  -> undefined
        "update":[]                 -> undefined
        _                           -> error "unsupported external command"

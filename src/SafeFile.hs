module SafeFile
    ( writeFileSafe
    ) where

import System.Directory

writeFileSafe :: FilePath -> String -> IO ()
writeFileSafe file content = do
    writeFile (file ++ ".tmp") content
    renameFile (file ++ ".tmp") file


module Deunicode where

import System.Directory
import System.FilePath
import Data.List
--import Data.String.Parse
import SafeFile
import Traverse

deunicode args = do
    let (dummy, files) = case args of
                            ("--dummy":l) -> (True, l)
                            l             -> (False, l)
    mapM_ (process dummy) files
  where dirCallback _ = return True
        fileCallback dummy file = do
            if takeExtension file == ".hs"
                then deUnicode dummy file
                else return ()

        process isDummy path = do
            isDir <- doesDirectoryExist path
            if isDir then processDir isDummy path
                     else deUnicode isDummy path
        processDir isDummy path = dirTraverse_ path (fileCallback isDummy) dirCallback

        deUnicode dummy file = do
            content <- readFile file
            let totalChars   = length content
                unicodeChars = howManyUnicodes content
                asciiContent = toAscii content
            if dummy
                then putStrLn ("  " ++ file ++ " : "  ++ show unicodeChars ++ " (total: " ++ show totalChars ++ ") after: " ++ show (howManyUnicodes asciiContent) ++ " " ++ getUnicodes asciiContent)
                else do
                    putStrLn ("  " ++ file ++ " : "  ++ show unicodeChars ++ " (total: " ++ show totalChars ++ ")")
                    writeFileSafe file asciiContent
                    writeFile (file ++ ".tmp") asciiContent
                    renameFile (file ++ ".tmp") file

        howManyUnicodes d = length $ filter (\c -> fromEnum c > 127) d
        getUnicodes d = sort $ filter (\c -> fromEnum c > 127) d

        toAscii []     = []
        toAscii (x:xs) =
            let z = case x of
                        '→' -> "->"
                        '⇒' -> "=>"
                        '←' -> "<-"
                        '∷' -> "::"
                        'α' -> "a"
                        'β' -> "b"
                        '∘' -> "."
                        '≡' -> "=="
                        '≠'  -> "/="
                        '⊕' -> "`mappend`" -- not the same precedence, so expect to have to fix by hands in some place
                        '≤' -> "<="
                        '∀' -> "forall"
                        '©' -> "(C)"
                        'μ' -> "mu"
                        'ν' -> "v"
                        'π' -> "pi"
                        _   -> [x]
             in z ++ toAscii xs

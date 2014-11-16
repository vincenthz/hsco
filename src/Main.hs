module Main where

import Control.Applicative
import System.Environment
import System.Directory
import System.FilePath
import Data.List
--import Data.String.Parse
import Data.Monoid
import SafeFile
import Lexer.Haskell (AtomType(..))
import HaskellOps
import Traverse

usage = error "usage: hsco <subcommand> [subopts]"

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

header allArgs = do
    case parseArgs Nothing [] allArgs of
        (Nothing      , _)        -> error "no template specified. use --template <template>"
        (_            , [])       -> error "no haskell files specified"
        (Just template, allFiles) -> do
            templateContent <- readFile template
            mapM_ (headerize templateContent) allFiles

  where parseArgs accTemplate accFiles args =
            case args of
                []                      -> (accTemplate, accFiles)
                "--template":template:r -> parseArgs (Just template) accFiles r
                f:r                     -> parseArgs accTemplate (f:accFiles) r
        headerize :: String -> FilePath -> IO ()
        headerize templateContent file = do
            (before, moduleStart) <- break isModule <$> readHaskellAtoms file
            let newTokens = [(Other, templateContent)]
                         ++ intersperse (Newline,"\n") (filter isPragma before)
                         ++ [(Newline,"\n")]
                         ++ moduleStart
            writeHaskellAtoms file newTokens
        isModule (Module,_) = True
        isModule _          = False
        isPragma (Pragma,_) = True
        isPragma _          = False

debugTokenize args = do
    case args of
        [] -> error "usage: hsco debug-tokenize <files..>"
        l  -> mapM_ tok l
  where tok f = readHaskellAtoms f >>= mapM_ (putStrLn . show)

{-
debugCompile args = do
  where
-}

data ImportOpts = ImportOpts
    { ioptReorder :: Bool
    , ioptList    :: Bool
    , ioptArgs    :: [String]
    }

instance Monoid ImportOpts where
    mempty = ImportOpts False False []
    mappend (ImportOpts a1 b1 c1) (ImportOpts a2 b2 c2) =
        ImportOpts (a1 || a2) (b1 || b2) (c1 ++ c2)

imports allArgs = do
    mapM_ findFileImports allArgs
  where findFileImports f = do
            tokens <- readHaskellAtoms f
            let importSyntax = importParseTokens tokens
            putStrLn $ show importSyntax

        importParseTokens ((Import,_): (Spaces,_): (Symbol,"qualified"): (Spaces,_): xs) =
            let (mn, r) = parseModuleName xs
             in (mn, True) : importParseTokens r
        importParseTokens ((Import,_): (Spaces,_): xs) =
            let (mn, r) = parseModuleName xs
             in (mn, False) : importParseTokens r
        importParseTokens (_:xs) =
            importParseTokens xs
        importParseTokens [] = []

        parseModuleName t = break isSpaces t

        isSpaces (Spaces,_)  = True
        isSpaces (Newline,_) = True
        isSpaces _           = False

help _ =
    mapM_ putStrLn
        [ "usage: hsco <subcommand> <subargs ...>"
        , ""
        , "  header --template <template-file> <file1> [file2 ..]"
        , ""
        , "  imports --list <file1> [file2 ..]"
        , "  imports --reorder <file1> [file2 ..]"
        , ""
        , "  move <module-name> <new-module-name>"
        , ""
        ]

main = do
    allArgs <- getArgs
    case allArgs of
        []       -> usage
        cmd:args ->
            case cmd of
                "-h"             -> help args
                "--help"         -> help args
                "help"           -> help args
                "deunicode"      -> deunicode args
                "header"         -> header args
                "imports"        -> imports args
                "debug-tokenize" -> debugTokenize args
                --"debug-compile"  -> debugCompile args
                _                -> error $ "unknown command " ++ cmd

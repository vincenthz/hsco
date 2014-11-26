module Main where

import System.Environment
--import Data.String.Parse
import Data.Monoid
import HaskellOps

import Header
import Deunicode

usage = error "usage: hsco <subcommand> [subopts]"

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
            putStrLn ("processing " ++ show f)
            tokens <- readHaskellAtoms f
            _ <- createChunks tokens
            {-
            let importSyntax = importParseTokens tokens
            -}
            --putStrLn ("chunks: " ++ show c)
            putStrLn "done"

        {-
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
        -}

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
        , "  deunicode <directory>"
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

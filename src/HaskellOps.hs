{-# LANGUAGE BangPatterns #-}
module HaskellOps
    ( parseHaskellAtoms
    , readHaskellAtoms
    , writeHaskellAtoms
    , createChunks
    , flattenChunks
    ) where

import           Control.Applicative
import           Control.Arrow (first)
import           Control.Monad (unless)
import           Control.Monad.IO.Class

import           Lexer.Haskell (Atom, AtomType(..), atomContent)
import qualified Lexer.Haskell as Lexer

import           SafeFile
import           View

-- A source file is logically 4 sections in the following order:
--
-- 1) the pre stuff: copyright, comment, language pragmas
-- 2) the module definition
-- 3) the import statements
-- 4) the module body

data SourceChunks = SourceChunks
    { sourcePre     :: [Atom String]
    , sourceModule  :: [Atom String]
    , sourceImports :: [Atom String]
    , sourceBody    :: [Atom String]
    } deriving (Show)

data ImportRestrict d =
      ImportHiding d
    | ImportShowing d
    | ImportNoRestrict
    deriving (Show,Eq)

data Import d = ImportSyn
    { importIsQualified :: Bool
    , importModule      :: d
    , importAs          :: Maybe d
    , importList        :: ImportRestrict d
    } deriving (Show,Eq)

parseHaskellAtoms :: String -> [Atom String]
parseHaskellAtoms content = Lexer.tokenize content

readHaskellAtoms :: FilePath -> IO ([Atom String])
readHaskellAtoms f = parseHaskellAtoms <$> readFile f

writeHaskellAtoms :: FilePath -> [Atom String] -> IO ()
writeHaskellAtoms file ts =
    writeFileSafe file $ concatMap atomContent ts

createChunks :: [Atom String] -> IO SourceChunks
createChunks atomsAll = do
    let (preSection, moduleStart) = break (atomIs Module) atomsAll
        (modSection, importStart) = break (atomIs Import) moduleStart 
    case importStart of
            [] ->
                -- if no import found, we expect the first stuff found after the module is the body
                --   error "un-implemented part of source chunks, cannot process without imports"
                error ("no imports: " ++ show preSection ++ "    " ++ show modSection ++ "     " ++ show importStart)
            _  -> do
                (imports, bodyStart) <- makeView (\x -> atomIs Spaces x || atomIs Newline x) importStart (getImports [])

                mapM_ (putStrLn . show) imports
                return $ SourceChunks preSection
                                      modSection
                                      []
                                      bodyStart
                
  where 
        getImports :: [Import [Atom String]] -> View [Import [Atom String]]
        getImports acc = do
            viewDebug "getImports"
            isImport <- viewNextEqConsume (Import, "import")
            liftIO $ putStrLn ("is import: " ++ show isImport)
            if isImport
                then do
                    importTokens <- consumeImport
                    getImports (importTokens : acc)
                else do
                    return (reverse acc)

        consumeImport :: View (Import [Atom String])
        consumeImport = do
            isQualified <- viewNextEqConsume (Symbol, "qualified")
            viewConsumeFiltered
            isSymbol <- viewNextIs Symbol
            symbol   <- viewNoFilterBreak spaceFilter

            viewDebug "consumeImport(as)"
            isAs <- viewNextEqConsume (Symbol, "as")
            asModule <- if isAs
                    then viewConsumeFiltered >> Just <$> viewNoFilterBreak spaceFilter
                    else return Nothing

            viewDebug "consumeImport(hiding)"
            isHiding <- viewNextEqConsume (Symbol, "hiding")
            hasList <- viewNextEqConsume (LParen, "(")
            listImport <- if hasList
                then Just <$> eatList
                else return Nothing
            return $ ImportSyn isQualified symbol asModule (ImportNoRestrict)

        spaceFilter x = atomIs Spaces x || atomIs Newline x

        eatList = loop 0
          where loop :: Int -> View ()
                loop !n = do
                    l <- viewPeek 1
                    case l of
                        []  -> return ()
                        x:_ -> do
                            viewConsume 1
                            case x of
                                (LParen, _)     -> loop (n+1)
                                (RParen, _)
                                    | n == 0    -> return ()
                                    | otherwise -> loop (n-1)
                                _               -> loop n

flattenChunks :: SourceChunks -> [Atom String]
flattenChunks sourceChunks = concat
    [ sourcePre sourceChunks
    , sourceModule sourceChunks
    , sourceImports sourceChunks
    , sourceBody sourceChunks
    ]

atomIs :: AtomType -> Atom a -> Bool
atomIs expectedTy (ty, _) = expectedTy == ty

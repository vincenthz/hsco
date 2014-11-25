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
        {-
        groupParens = first reverse . loop [(LParen, "(")] 0
          where loop :: [Atom String] -> Int -> [Atom String] -> ([Atom String], [Atom String])
                loop acc _    []     = (acc, [])
                loop acc !lvl (x:xs) =
                    case x of
                        (LParen, _)     -> loop (x:acc) (lvl+1) xs
                        (RParen, _)
                            | lvl == 0  -> (x:acc, xs)
                            | otherwise -> loop (x:acc) (lvl-1) xs
                        _               -> loop (x:acc) lvl xs
        -}

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
    {-
                    x <- viewPeek 1
                    --putStrLn $ show x
                    viewConsume 1
                    getImports acc
-}

        -- split until a specific atom type has been found
        {-
        splitUntilAfter :: AtomType -> [Atom a] -> ([Atom a], [Atom a])
        splitUntilAfter expectedTy = reverse . loop []
          where loop acc []            = (acc, [])
                loop acc (x@(ty,_):xs)
                    | expectedTy == ty = (x:acc, xs)
                    | otherwise        = loop (x:acc) xs
        -}

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
                then undefined -- eat stuff
                else return Nothing
            return $ ImportSyn isQualified symbol asModule (ImportNoRestrict)

        spaceFilter x = atomIs Spaces x || atomIs Newline x
{-
        -- import [qualified] ...
        getImport t@((Import,_): (Spaces,_): (Symbol,"qualified"): (Spaces,_): xs) =
            getImportMN (reverse $ take 4 t) True xs
        getImport t@((Import,_): (Spaces,_): xs) =
            getImportMN (reverse $ take 2 t) False xs

        -- module name ...
        getImportMN acc isQualified xs =
            let (moduleName, r) = break (atomIs Spaces) xs
             in getImportAs (reverse moduleName++acc) (isQualified, moduleName) r
        -- as ...
        getImportAs acc (isQualified, moduleName) t@((Spaces,_): (Symbol,"as"): (Spaces,_):xs) =
            let (asName, r) = break (atomIs Spaces) xs
             in getImportList (reverse (take 3 t) ++ acc) (isQualified, moduleName, Just asName)
        getImportAs acc (isQualified, moduleName) xs =
            getImportList acc (isQualified, moduleName, Nothing) xs
        -- [ [hiding] (...) ]
        getImportList acc (isQualified, mn, as) t@((Symbol, "hiding"): (Spaces,_): (LParen,_):xs) =
            let (inside, r) = groupParens xs
             in (reverse acc ++ inside, (isQualified, mn, as, ImportHiding undefined), xs)
        getImportList acc (isQualified, mn, as) ((LParen,_):xs) =
            let (inside, r) = groupParens xs
             in (reverse acc ++ inside, (isQualified, mn, as, ImportShowing inside), xs)
        getImportList acc (isQualified, mn, as) xs =
            (reverse acc, (isQualified, mn, as, ImportNoRestrict), xs)
-}

flattenChunks :: SourceChunks -> [Atom String]
flattenChunks sourceChunks = concat
    [ sourcePre sourceChunks
    , sourceModule sourceChunks
    , sourceImports sourceChunks
    , sourceBody sourceChunks
    ]

atomIs :: AtomType -> Atom a -> Bool
atomIs expectedTy (ty, _) = expectedTy == ty

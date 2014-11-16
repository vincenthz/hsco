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

import           Lexer.Haskell (Atom, AtomType(..), atomContent)
import qualified Lexer.Haskell as Lexer

import           SafeFile

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

data Import d = ImportExploded
    { importIsQualified :: Bool
    , importModule      :: d
    , importAs          :: d
    , importList        :: ImportRestrict d
    } deriving (Show,Eq)

parseHaskellAtoms :: String -> [Atom String]
parseHaskellAtoms content = Lexer.tokenize content

readHaskellAtoms :: FilePath -> IO ([Atom String])
readHaskellAtoms f = parseHaskellAtoms <$> readFile f

writeHaskellAtoms :: FilePath -> [Atom String] -> IO ()
writeHaskellAtoms file ts =
    writeFileSafe file $ concatMap atomContent ts

createChunks :: [Atom String] -> SourceChunks
createChunks atomsAll =
    let (preSection, moduleStart) = break (atomIs Module) atomsAll
        (modSection, importStart) = break (atomIs Import) moduleStart 
     in case importStart of
            [] -> -- if no import found, we expect the first stuff found after the module is the body
                    error "un-implemented part of source chunks, cannot process without imports"
            _  ->
                let (imports, bodyStart) = getImports importStart
                 in SourceChunks preSection
                                 modSection
                                 imports
                                 bodyStart
                
  where groupParens = first reverse . loop [(LParen, "(")] 0
          where loop :: [Atom String] -> Int -> [Atom String] -> ([Atom String], [Atom String])
                loop acc _    []     = (acc, [])
                loop acc !lvl (x:xs) =
                    case x of
                        (LParen, _)     -> loop (x:acc) (lvl+1) xs
                        (RParen, _)
                            | lvl == 0  -> (x:acc, xs)
                            | otherwise -> loop (x:acc) (lvl-1) xs
                        _               -> loop (x:acc) lvl xs

{-
        getImports (x@(Import,_):xs) =
        getImports (x@(Other,"#"):xs) =
-}

        -- import [qualified] ...
        getImport t@((Import,_): (Spaces,_): (Symbol,"qualified"): (Spaces,_): xs) =
            getImportsMN (reverse $ take 4 t) True xs
        getImport t@((Import,_): (Spaces,_): xs) =
            getImportsMN (reverse $ take 2 t) False xs

        -- module name ...
        getImportMN acc isQualified xs =
            let (moduleName, r) = break isSpaces t
             in getImportAs (reverse moduleName++acc) (isQualified, moduleName) r
        -- as ...
        getImportAs acc (isQualified, moduleName) t@((Spaces:_): (Symbol,"as"): (Spaces,_):xs) =
            let (asName, r) = break isSpaces xs
             in getImportList (reverse (take 3 t) ++ acc) (isQualified, moduleName, Just asName)
        getImportAs acc (isQualified, moduleName) xs =
            getImportList acc (isQualified, moduleName, Nothing) xs
        -- [ [hiding] (...) ]
        getImportList acc (isQualified, mn, as) t@((Symbol, "hiding"): (Spaces,_): (LParen,_):xs) =
            let (inside, r) = groupParen xs
             in (reverse acc ++ inside, (isQualified, mn, as, ImportHiding undefined), xs)
        getImportList acc (isQualified, mn, as) (LParen,_):xs =
            let (inside, r) = groupParen xs
             in (reverse acc ++ inside, (isQualified, mn, as, ImportShowing inside), xs)
        getImportList acc (isQualified, mn, as) xs =
            (reverse acc, (isQualified, mn, as, ImportNoRestrict), xs)

        getImports (_:xs) =
            getImports xs
        getImports [] = []

flattenChunks :: SourceChunks -> [Atom String]
flattenChunks sourceChunks = concat
    [ sourcePre sourceChunks
    , sourceModule sourceChunks
    , sourceImports sourceChunks
    , sourceBody sourceChunks
    ]

atomIs :: AtomType -> Atom a -> Bool
atomIs expectedTy (ty, _) = expectedTy == ty

-- Haskell tokenizer
{-# LANGUAGE BangPatterns #-}
module Lexer.Haskell
    ( AtomType(..)
    , Atom
    , tokenize
    , atomContent
    ) where

import Control.Arrow (first)
import Data.Char

-- | number of character skipped, and the remaining string
type Skip = (Int, String)

data AtomType =
      LineComment
    | BlockComment
    | Pragma
    | Str
    | Char
    | Module
    | Import
    | Symbol
    | Digits
    | LParen
    | RParen
    | Newline
    | Spaces
    | Other
    deriving (Show,Eq)

type Atom a = (AtomType, a)

atomContent :: Atom String -> String
atomContent (_, s) = s

nextIsNonAlpha :: String -> Bool
nextIsNonAlpha []    = True
nextIsNonAlpha (x:_) = isAlpha x

tokenize :: String -> [Atom String]
tokenize [] = []
tokenize s  =
    let (token, left) =
            case s of
                'i':'m':'p':'o':'r':'t':xs
                    | nextIsNonAlpha xs -> takeAndMap Import (\_ -> (6, xs)) s
                    | otherwise         -> eatSymbol s
                'm':'o':'d':'u':'l':'e':xs
                    | nextIsNonAlpha xs -> takeAndMap Module (\_ -> (6, xs)) s
                    | otherwise         -> eatSymbol s
                '{':'-':'#':_   -> takeAndMap Pragma (eatPragma 0) s
                '{':'-':_       -> takeAndMap BlockComment (eatBlockComment 0 0) s
                '-':'-':_       -> takeAndMap LineComment (eatLineComment 0) s
                '"':_           -> takeAndMap Str eatString s
                '\'':_          -> takeAndMap Char eatChar s
                '\n':xs         -> ((Newline, "\n"), xs)
                '(':xs          -> ((LParen, "("), xs)
                ')':xs          -> ((RParen, ")"), xs)
                (c:xs)
                    | isAlpha c -> eatSymbol s
                    | isDigit c -> first (atomOf Digits) $ span isDigit s
                    | isSpace c -> first (atomOf Spaces) $ span isSpace s
                    | otherwise -> ((Other, [c]), xs)
                _               -> error "internal error"
     in token : tokenize left
  where takeAndMap :: AtomType -> (String -> Skip) -> String -> (Atom String, String)
        takeAndMap constr eat = first (\n -> (constr, take n s)) . eat

        atomOf :: AtomType -> String -> Atom String
        atomOf ty atomS = (ty,atomS)

        eatPragma !n ('#':'-':'}':xs) = (n+3, xs)
        eatPragma !n (_:xs)           = eatPragma (n+1) xs
        eatPragma !n []               = (n, [])

        eatSymbol = first (atomOf Symbol) . span (\c -> isAlpha c || c == '_')

        eatBlockComment :: Int -> Int -> String -> Skip
        eatBlockComment !n !lvl ('{':'-':xs) =
            eatBlockComment (n+2) (lvl+1) xs
        eatBlockComment !n !lvl ('-':'}':xs)
            | lvl == 1  = (n+2, xs)
            | otherwise = eatBlockComment (n+2) (lvl-1) xs
        eatBlockComment n lvl (_:xs) = eatBlockComment (n+1) lvl xs
        eatBlockComment n _   []     = (n,[])

        eatLineComment :: Int -> String -> Skip
        eatLineComment !n ('\n':xs) = (n+1, xs)
        eatLineComment !n (_:xs)    = eatLineComment (n+1) xs
        eatLineComment !n []        = (n, [])

        -- TODO this doesn't supported escaped quote.
        eatString = loop 1 . drop 1
          where loop n ('"':xs) = (n+1, xs)
                loop n ('\\':_:xs) = loop (n+2) xs
                loop n (_:xs)    = loop (n+1) xs
                loop n []        = (n,[])

        eatChar = loop 1 . drop 1
          where loop n ('\\':'\'':xs) = loop (n+2) xs
                loop n ('\'':xs)      = (n+1, xs)
                loop n (_:xs)         = loop (n+1) xs
                loop n []             = (n,[])

{-
parseSource :: String -> Source
parseSource content = Source content (tokenize content)

readSource :: FilePath -> IO Source
readSource file = parseSource <$> readFile file

writeSource :: FilePath -> Source -> IO ()
writeSource file (Source content _) = writeFileSafe file content

writeTokens :: FilePath -> [Token] -> IO ()
writeTokens file ts = writeFileSafe file $ concatMap tokenContent ts

writeSourceModule :: Source -> IO ()
writeSourceModule = undefined
-}

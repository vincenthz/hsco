module CabalFormat where

import System.Environment

import Data.Char (isDigit, isAlpha, isAlphaNum, toLower)
import Data.List (isPrefixOf, find)
--import qualified Data.ByteString as B

data Operator = Or | And | Lt | Le | Gt | Ge | Eq
    deriving (Show,Eq)
data AtomType = Number
              | Version
              | Symbol
              | Comment
              | Space
              | Newline
              | Colon
              | Comma
              | Lparen
              | Rparen
              | Punct Operator
              | Other
              deriving (Show,Eq)
type Atom a = (Int, AtomType, a)

-- | consume the next atom from an input with the previous indentation level
-- and return the indentation level, the atom parsed and the remaining input.
next :: Int -> String -> (Int, Atom String, String)
next lvl s@(c:cs)
    | isSpace c         = eatSpace
    | isNewline c       = (0, (lvl, Newline, [c]), cs)
    | isDigit c         = eatNumberish
    | isPrefixOf "--" s = eatComment
    | isAlpha c         = eatSymbol
    | c == ':'          = (lvl, (lvl, Colon, [c]), cs)
    | c == ','          = (lvl, (lvl, Comma, [c]), cs)
    | otherwise         =
        case s of
            '(':s'     -> (lvl, (lvl, Lparen, "("), s')
            ')':s'     -> (lvl, (lvl, Rparen, ")"), s')
            '&':'&':s' -> (lvl, (lvl, Punct And, "&&"), s')
            '|':'|':s' -> (lvl, (lvl, Punct Or, "||"), s')
            '=':'=':s' -> (lvl, (lvl, Punct Eq, "=="), s')
            '>':'=':s' -> (lvl, (lvl, Punct Ge, ">="), s')
            '>':s'     -> (lvl, (lvl, Punct Gt, ">"), s')
            '<':'=':s' -> (lvl, (lvl, Punct Le, "<="), s')
            '<':s'     -> (lvl, (lvl, Punct Lt, "<"), s')
            _          -> (lvl, (lvl, Other, [c]), cs)
  where eatSpace     = let (s1, s2) = break (not . isSpace) s
                        in (if lvl == 0 then length s1 else lvl, (lvl, Space, s1), s2)
        -- rewrite using breakPlus1 to eat '\n' directly.
        eatComment = let (s1,s2) = break (== '\n') s
                      in (0, (lvl, Comment, s1 ++ "\n"), drop 1 s2)

        eatNumberish = let (s1,s2) = break (not . isNumberish) s
                          in case find (\v -> v == '*' || v == '.') s1 of
                                Nothing -> (lvl, (lvl, Number, s1), s2)
                                Just _  -> (lvl, (lvl, Version, s1), s2)
        eatSymbol = let (s1,s2) = break (not . isSymbolTail) cs
                     in (lvl, (lvl, Symbol, c:s1), s2)
        isNumberish v = isDigit v || v == '*' || v == '.'
        isSpace v     = v == ' '
        isNewline v   = v == '\n'
        isSymbolTail v = isAlphaNum v || v `elem` "-@#$%/?!."

-- eat a serie of token at a certain level, and return
-- the group and the uneaten atoms.
eatGroup :: Int -> [Atom String] -> ([Atom String], [Atom String])
eatGroup _   []     = ([], [])
eatGroup lvl (x1@(_,Newline,_):xs) =
    case getNextNonSpace xs of
        Nothing           -> ([x1], [])
        Just (lvl2, _, _)
            | lvl2 > lvl  -> let (r1, r2) = eatGroup lvl xs in (x1:r1, r2)
            | otherwise   -> ([x1], xs)
eatGroup lvl (x:xs) =
    let (r1,r2) = eatGroup lvl xs
     in (x:r1, r2)

getNextNonSpace :: [Atom a] -> Maybe (Atom a)
getNextNonSpace []               = Nothing
getNextNonSpace ((_,Space,_):xs) = getNextNonSpace xs
getNextNonSpace (x:_)            = Just x

getAtoms :: String -> [Atom String]
getAtoms content = loop 0 content
  where loop currentLevel s
            | null s    = []
            | otherwise =
                let (lvl, tok, s') = next currentLevel s
                 in tok:loop lvl s'

fusion :: [Atom String] -> String
fusion []        = []
fusion ((_,_,s):r) = s ++ fusion r

group :: [Atom String] -> [[Atom String]]
group = loop 0
  where loop _   [] = []
        loop lvl l  =
            let (g1,g2) = eatGroup lvl l
             in g1 : loop lvl g2

getField :: String -> [Atom String] -> ([Atom String], [Atom String])
getField fieldName l = loop l
  where loop []             = ([],[])
        loop ((lvl, Symbol, s):(_,Colon,_):xs)
            | fieldName `noCaseEq` s = eatGroup (lvl+1) xs
            | otherwise              = loop xs
        loop (_:xs) = loop xs

getAllFields :: String -> [Atom String] -> [[Atom String]]
getAllFields _ [] = []
getAllFields s l  =
    let (f1,r) = getField s l
     in f1 : getAllFields s r

noCaseEq :: String -> String -> Bool
noCaseEq s1 s2 = map toLower s1 == map toLower s2

{-
main :: IO ()
main = do
    args    <- getArgs
    content <- readFile (args !! 0)
    let atoms = getAtoms content
    --unless (fusion atoms == content) $ error "not equal"

    --mapM_ (putStrLn . show) $ group atoms

    --let g = getField "version" atoms
    mapM_ (putStrLn . show) $ getAllFields "build-depends" atoms
-}

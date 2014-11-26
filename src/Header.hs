module Header (header) where

import Control.Applicative
import Data.Char
import Data.List
import Lexer.Haskell (AtomType(..))
import HaskellOps

data TAtom = Text String | Var String deriving (Show)
type Template = [TAtom]

header allArgs = do
    case parseArgs Nothing [] allArgs of
        (Nothing      , _)        -> error "no template specified. use --template <template>"
        (_            , [])       -> error "no haskell files specified"
        (Just template, allFiles) -> do
            templateContent <- parseTemplate <$> readFile template
            mapM_ (headerize templateContent) allFiles

  where parseArgs accTemplate accFiles args =
            case args of
                []                      -> (accTemplate, accFiles)
                "--template":template:r -> parseArgs (Just template) accFiles r
                f:r                     -> parseArgs accTemplate (f:accFiles) r
        headerize :: Template -> FilePath -> IO ()
        headerize template file = do
            let templateContent = renderTemplate template []
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

renderTemplate :: Template -> [(String,String)] -> String
renderTemplate template attrs =
    concat $ map renderAtom template
  where
        renderAtom :: TAtom -> String
        renderAtom (Text b) = b
        renderAtom (Var s)  = maybe "" id $ lookup s attrs

parseTemplate :: String -> Template
parseTemplate content 
    | null content        = []
    | head content == '$' = parseVar $ tail content
    | otherwise           = parseText content
  where
        parseText :: String -> Template
        parseText s
            | null s    = []
            | otherwise = Text b : (parseVar $ tailSafe a)
          where
                (b, a) = break ((==) '$') s

        parseVar :: String -> Template
        parseVar s
            | null s    = []
            | otherwise =
                let (b, a) = break ((==) '$') s in
                if isVariable b
                    then Var b  : (parseText $ tailSafe a)
                    else Text b : (parseVar $ tailSafe a)

        isVariable :: String -> Bool
        isVariable = and . map isVariableChar
          where isVariableChar :: Char -> Bool
                isVariableChar c = isAlpha c || isDigit c

        tailSafe s
            | null s    = []
            | otherwise = tail s

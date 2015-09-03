{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module View
    ( View
    , makeView
    , viewPeek
    , viewConsume
    , viewConsumeFiltered
    , viewNextIs
    , viewNextEq
    , viewNextEqConsume
    , viewNoFilterBreak
    , viewRecord
    , viewFlush
    , viewDebug
    ) where

import Control.Arrow (first, second)
import Control.Applicative
import Control.Monad.State
import Data.List (intercalate)
import Lexer.Haskell (Atom, AtomType(..))
import Prelude hiding (splitAt)

data ViewState = ViewState
    { viewCurrent     :: [Atom String]
    , viewEaten       :: [Atom String]
    , viewIsRecording :: Bool
    , viewRecord      :: [Atom String]
    , viewFilter      :: Atom String -> Bool
    }

newtype View a = View { runView :: StateT ViewState IO a }
    deriving (Functor,Applicative,Monad,MonadState ViewState,MonadIO)

makeView :: (Atom String -> Bool) -> [Atom String] -> View a -> IO (a, [Atom String])
makeView filterF current viewF =
    second viewCurrent <$> runStateT (runView viewF) st
  where st = ViewState { viewCurrent     = current
                       , viewEaten       = []
                       , viewIsRecording = False
                       , viewRecord      = []
                       , viewFilter      = filterF }

splitAt :: Bool -> (Atom String -> Bool) -> Int -> [Atom String] -> ([Atom String], [Atom String])
splitAt keepFiltered filterF n = first reverse . loop [] 0
  where loop acc _ [] = (acc, [])
        loop acc i l@(x:xs)
            | i == n    = (acc, l)
            | filterF x = if keepFiltered then loop (x:acc) n xs else loop acc n xs
            | otherwise = loop (x:acc) (n+1) xs

takeFiltered filterF n l = fst $ splitAt False filterF n l

viewPeek :: Int -> View [Atom String]
viewPeek n = do
    st <- get
    return $ takeFiltered (viewFilter st) n $ viewCurrent st

viewConsume :: Int -> View ()
viewConsume n = do
    st <-  get
    let (l,r) = splitAt True (viewFilter st) n $ viewCurrent st
    put $ st { viewCurrent = r, viewEaten = reverse l ++ viewEaten st }

viewConsumeFiltered :: View ()
viewConsumeFiltered = do
    st <- get
    let (l,r) = span (viewFilter st) $ viewCurrent st
    put $ st { viewCurrent = r, viewEaten = reverse l ++ viewEaten st }

viewNextIs :: AtomType -> View Bool
viewNextIs expectedTy = do
    st <-  get
    case takeFiltered (viewFilter st) 1 $ viewCurrent st of
        []         -> return False
        ((ty,_):_) -> return (expectedTy == ty)

viewNextEq :: Atom String -> View Bool
viewNextEq expectedAtom = do
    st <- get
    case takeFiltered (viewFilter st) 1 $ viewCurrent st of
        []       -> return False
        (atom:_) -> return (expectedAtom == atom)

viewNextEqConsume :: Atom String -> View Bool
viewNextEqConsume expectedAtom = do
    st <-  get
    case takeFiltered (viewFilter st) 1 $ viewCurrent st of
        []       -> liftIO (putStrLn ("uhm: " ++ show (splitAt True (viewFilter st) 1 $ viewCurrent st))) >> return False
        (atom:_) | expectedAtom == atom -> liftIO (putStrLn ("matched " ++ show expectedAtom)) >> viewConsume 1 >> return True
                 | otherwise            -> liftIO $ putStrLn ("viewNextEqConsume: " ++ show atom ++ " was expecting " ++ show expectedAtom) >> return False

viewNoFilterBreak :: (Atom String -> Bool) -> View [Atom String]
viewNoFilterBreak stopPoint = do
    st <-  get
    let (l,r) = break stopPoint $ viewCurrent st
    --liftIO $ putStrLn ("noFilterBreak: " ++ show l ++ " <=> " ++ show (take 3 r))
    put $ st { viewCurrent = r, viewEaten = reverse l ++ viewEaten st }
    return l

viewFlush :: View [Atom String]
viewFlush = do
    st <- get
    put $ st { viewRecord = [] }
    return $ viewRecord st

viewDebug :: String -> View ()
viewDebug s = do
    st <- get
    liftIO $ do
        putStrLn $ s ++ " ==> " ++ (flattenToken $ reverse $ take 2 $ viewEaten st) ++ " *** " ++ (flattenToken $ take 5 $ viewCurrent st)
  where flattenToken = intercalate " | " . map (show . snd)

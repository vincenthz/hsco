#!/usr/bin/env stack
-- stack --resolver lts-10.8 script --package foundation --package basement --package memory --package directory --package cryptonite
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}

import           Data.Char
import           Data.List hiding (head)
import           System.Environment
import           Control.Monad

countLine =
      length
    . filter (\s -> not (null s || all isSpace s))
    . map stripComment
    . lines
  where
    stripComment :: String -> String
    stripComment []              = []
    stripComment ('-':'-':' ':_) = []
    stripComment (x:xs)          = x:stripComment xs


pad n s
    | n <= len  = s
    | otherwise = replicate (n - len) ' ' ++ s
  where len = length s

doOne acc file = do
    l <- countLine <$> readFile file
    putStrLn (pad 6 (show l) ++ " " ++ file)
    pure $! acc+l

main = do
    args <- getArgs
    total <- foldM doOne 0 args
    putStrLn $ pad 6 (show total)

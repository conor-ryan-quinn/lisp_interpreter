module Main where

import System.Environment

import IO

main :: IO ()
main = do
     args <- getArgs
     if null args then runRepl else runOne $ args

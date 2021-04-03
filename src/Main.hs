module Main where

import System.Environment ( getArgs )
import System.IO ( readFile )

import Ast ( Program )
import Parser ( executeParser, Parser(runParser) )
import ProgramParser ( parseProgram )

main :: IO ()
main = do
    args     <- getArgs
    contents <- traverse readFile args
    print $ compile <$> contents

compile :: String -> Maybe Program
compile = executeParser parseProgram
module Main where

import Control.Monad ( when )
import Data.Maybe ( isNothing )
import System.Environment ( getArgs )
import System.Exit ( ExitCode(ExitFailure), exitWith )
import System.FilePath ( extSeparator, takeBaseName )
import System.IO ( readFile )

import Ast ( Program )
import Assembly (asAssembly )
import Parser ( executeParser, Parser(runParser) )
import Pretty ( prettyPrint )
import ProgramParser ( parseProgram )

main :: IO ()
main = do
    args     <- getArgs
    when (length args /= 1) $ exitWith $ ExitFailure 1
    let inputFile = head args
    content <- readFile inputFile
    let result = parse content
    when (isNothing result) $ exitWith $ ExitFailure 2
    let (Just program) = result
    let outputFile = takeBaseName inputFile ++ [extSeparator, 's']
        assembly   = compile program
    putStrLn $ format program
    writeFile outputFile assembly

parse :: String -> Maybe Program
parse = executeParser parseProgram

compile :: Program -> String
compile = asAssembly

format :: Program -> String
format = show . prettyPrint

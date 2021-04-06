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
import ProgramParser ( parseProgram )

main :: IO ()
main = do
    args     <- getArgs
    when (length args /= 1) $ exitWith $ ExitFailure 1
    let inputFile = head args
    content <- readFile inputFile
    let result = compile content
    when (isNothing result) $ exitWith $ ExitFailure 2
    let outputFile = takeBaseName inputFile ++ [extSeparator, 's']
        (Just assembly) = result
    writeFile outputFile assembly


compile :: String -> Maybe String
compile = fmap asAssembly <$> executeParser parseProgram
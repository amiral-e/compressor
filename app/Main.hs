--
-- EPITECH PROJECT, 2022
-- Untitled (Workspace)
-- File description:
-- Main
--

module Main where

import Lib ( defaultConf, getConf, imageCompressor )
import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(ExitFailure) )

main :: IO ()
main = do
    args <- getArgs
    case getConf args defaultConf of
        Just conf -> imageCompressor conf
        Nothing -> exitWith (ExitFailure 84)

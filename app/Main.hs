module Main where

import Control.Monad.State

import GameState
import GameIO

main :: IO ()
main = do
    putStrLn "Hello Functional Adventure (TF2 Style)!"
    putStrLn "Your objective is to get the golden frying pan and bring it to TC Hydro!"
    evalStateT (forever repl) initialState
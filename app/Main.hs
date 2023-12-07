module Main where

import MyLib

import Control.Monad.State

main :: IO ()
main = do
    let (outputs, finalState) = runState (turnstile exampleInput) Locked
    putStrLn "Outputs:"
    print outputs
    putStrLn "Final State:"
    print finalState

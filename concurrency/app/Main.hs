module Main (main) where

import Lib
import Control.Concurrent

main :: IO ()
main = do 
    putStrLn "hello world!"

{-| make a message list the shared variable so that a user-thread takes it and adds a message to it that way each user can check if the list is 100 long
    if it is the program can terminate.
    -}
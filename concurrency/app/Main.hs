module Main (main) where

import Lib
import Control.Concurrent
import GHCi.Message (Message)
import Distribution.Compat.Prelude (undefined)

main :: IO ()
main = do 
    putStrLn "hello world!"

{-| TODO: make a message list the shared variable so that a user-thread takes it and adds a message to it that way each user can check if the list is 100 long
    if it is the program can terminate. 
    -}

userProcess :: User -> MVar [Message] -> IO ()
userProcess = undefined
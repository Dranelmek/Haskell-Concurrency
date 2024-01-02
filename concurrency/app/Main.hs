module Main (main) where

import Lib
import Control.Concurrent

main :: IO ()
main = do 
    putStrLn "hello world!"

{-| TODO: make a message list the shared variable so that a user-thread takes it and adds a message to it that way each user can check if the list is 100 long
    if it is the program can terminate. 

    add receiveMessage function
    -}

-- | create user thread
userProcess :: User -> MVar Message -> MVar [Message] -> IO ()
userProcess = undefined

-- | manipulate shared memory to send message
sendMessage :: MVar Message -> MVar [Message] -> IO ()
sendMessage = undefined
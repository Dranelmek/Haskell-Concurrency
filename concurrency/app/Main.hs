
module Main (main) where

import Lib
import Control.Concurrent
main :: IO ()
main = do 
    let p = User {username = "tester1", messagesSent = 1, messagesRecieved = 0}
    let q = User {username = "tester2", messagesSent = 0, messagesRecieved = 1}
    let test = Message "Hello world" p q
    putStrLn $ show test

{-| TODO: make a message list the shared variable so that a user-thread takes it and adds a message to it that way each user can check if the list is 100 long
    if it is the program can terminate. 

    add receiveMessage function
    -}

names = ["James","Robert","John","Michael","David","Mary","Patricia","Jennifer","Linda","Elizabeth"]

-- | create user thread
userProcess :: User -> MVar Message -> MVar [Message] -> IO ()
userProcess = undefined

-- | manipulate shared memory to send message
sendMessage :: MVar Message -> MVar [Message] -> IO ()
sendMessage = undefined

repeatProcess :: Int -> IO ()
repeatProcess a = if a < 0 then return () else do
    putStrLn $ show a
    repeatProcess (a - 1)
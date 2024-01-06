
module Main (main) where

import Lib
import Control.Concurrent
import System.Random
main :: IO ()
main = do 
    r <- randomRIO (0,9) :: IO Int
    putStrLn $ (show r) ++ " " ++ (names!!r)


{-| TODO: make a message list the shared variable so that a user-thread takes it and adds a message to it that way each user can check if the list is 100 long
    if it is the program can terminate. 

    add receiveMessage function
    -}

names = ["James","Robert","John","Michael","David","Mary","Patricia","Jennifer","Linda","Elizabeth"]

-- | create user thread
userProcess :: User -> MVar Message -> MVar [Message] -> IO ()
userProcess user letterBox messageList = undefined

-- | manipulate shared memory to send message
sendMessage :: MVar Message -> MVar [Message] -> IO ()
sendMessage = undefined

repeatProcess :: [String] -> IO ()
repeatProcess [] = return ()
repeatProcess (x:xs) = do
    putStrLn x
    repeatProcess xs
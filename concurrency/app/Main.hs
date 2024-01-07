
module Main (main) where

import Lib
import Control.Concurrent
import System.Random
main :: IO ()
main = do 
    -- r <- randomRIO (0,9) :: IO Int
    -- sendDecision <- randomIO :: IO Bool
    -- putStrLn $ (show r) ++ " " ++ (names!!r) ++ " " ++ (show sendDecision)
    out <- chooseName "James" names

    putStrLn out


{-| TODO: make a message list the shared variable so that a user-thread takes it and adds a message to it that way each user can check if the list is 100 long
    if it is the program can terminate. 

    add receiveMessage function
    -}

names = ["James","Robert","John","Michael","David","Mary","Patricia","Jennifer","Linda","Elizabeth"]

-- | create user thread
userProcess :: User -> MVar Message -> MVar [Message] -> IO ()
userProcess user letterBox messageList = do
    -- | initialise random variables
    delay <- randomRIO (0,100) :: IO Int
    sendDecision <- randomIO :: IO Bool
    -- | Check if the user received a message
    currentMessage <- takeMVar letterBox
    if (target currentMessage) /= (username user) then 
        putMVar letterBox currentMessage
        else
            undefined
            -- if the target name matches the user's name we update the message's recipent field and add it to the message list
    mList <- takeMVar messageList
    -- | send a message to a random user
    
    threadDelay delay
    

-- | manipulate shared memory to send message
sendMessage :: MVar Message -> MVar [Message] -> IO ()
sendMessage = undefined

repeatProcess :: [String] -> IO ()
repeatProcess [] = return ()
repeatProcess (x:xs) = do
    putStrLn x
    repeatProcess xs

-- | chooses a random name from the given list that is not the name given as an argument
chooseName :: String -> [String] -> IO String
chooseName _ [] =  return ""
chooseName name list = do 
    r <- randomRIO (0,9) :: IO Int
    let out = (list!!r)
    if out /= name then
        return out
        else
            chooseName name list


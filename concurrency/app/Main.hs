{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}

module Main (main) where

import Lib
import Control.Concurrent
import System.Random
main :: IO ()
main = do 
    letterBox <- newEmptyMVar
    messageList <- newMVar []
    generateUsers names letterBox messageList
    endProcess messageList


{-| TODO: solve empty-letterbox-deadlock
    Possible solution: each userprocess creates 2 sub threads one for receiving and one for sending...
    the super thread for each user can then kill the subthreads after sleeping for a bit.
    -}

names = ["James","Robert","John","Michael","David","Mary","Patricia","Jennifer","Linda","Elizabeth"]
dummyUser = User "" 0 0

-- | create user thread
userProcess :: User -> MVar Message -> MVar [Message] -> IO ()
userProcess user letterBox messageList = do
    -- | initialise random variables
    delay <- randomRIO (0,100) :: IO Int

    -- | check if the max number of messages has been sent
    allMessages <- readMVar messageList
    if length allMessages < 100 then do
        -- | check if the user received a message
        messageRecieved <- receiveMessage user letterBox messageList
        
        -- | send a message to a random user
        messageSent <- sendMessage user letterBox
        
        -- | update the user's fields
        let updatedUser = User (username user) (messagesSent user + boolToInt messageSent) (messagesRecieved user + boolToInt messageRecieved)
        
        -- | sleep for a random amount of time
        threadDelay delay
        
        -- | restart the thread
        userProcess updatedUser letterBox messageList
        else
            putStrLn $ show user

-- | checks the shared memory if a message was sent to the user, then updates the list of all messages
receiveMessage :: User -> MVar Message -> MVar [Message] -> IO Bool
receiveMessage user letterBox messageList = do
    currentMessageRead <- readMVar letterBox
    if target currentMessageRead == username user then do
        currentMessage <- takeMVar letterBox
        updateList <- takeMVar messageList
        let signedMessage = Message (content currentMessage) (sender currentMessage) (target currentMessage) user
        putStrLn $ show signedMessage
        putMVar messageList $ updateList ++ [signedMessage]
        return True
        else
            return False


-- | wrapper function that handles ruterning thread ID and send/receive bool and also kills the thread after the main thread has slept for a bit
-- TODO: research how to kill a thread.

-- | manipulate shared memory to send message
sendMessage :: User -> MVar Message -> IO Bool
sendMessage user letterBox = do
    sendDecision <- randomIO :: IO Bool
    if sendDecision then do
        targetUser <- chooseName (username user) names
        putMVar letterBox $ Message ("Hello "++targetUser++"!") user targetUser dummyUser
        return sendDecision
        else
            return sendDecision


generateUsers :: [String] -> MVar Message -> MVar [Message] -> IO ()
generateUsers [] _ _ = return ()
generateUsers (x:xs) letterBox messageList= do
    _ <- forkIO (userProcess (User x 0 0) letterBox messageList)
    generateUsers xs letterBox messageList

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

boolToInt :: Bool -> Int
boolToInt a | a = 1 | otherwise = 0

endProcess :: MVar [Message] -> IO ()
endProcess messageList = do
    allMessages <- readMVar messageList
    if length allMessages < 100 then do
        threadDelay 10
        endProcess messageList
        else
            return ()

{-
Things I found:
challenges: 
The system could deadlock in various situations:
- at the start if there is no sent message in the system and all threads try to read a message
- if there is a message to a certain thread and that thread is trying to send a message.
Solution:
- make the threads only wait for a certain time before moving on with their life

the prototype is done but the next step is figuring out how to beat the afromentioned deadlock.
-}
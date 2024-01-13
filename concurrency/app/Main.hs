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
    localThreadStorage <- newMVar (False,False)

    -- | check if the max number of messages has been sent
    allMessages <- readMVar messageList
    if length allMessages < 100 then do
        -- | check if the user received a message
        rTid <- forkIO (receiveMessage user letterBox messageList localThreadStorage)
        
        -- | send a message to a random user
        sTid <- forkIO (sendMessage user letterBox localThreadStorage)
        
        -- | sleep the superthread while the subthreads work then kill both subthreads
        threadDelay 100
        killThread rTid
        killThread sTid

        -- | update the user's fields
        userActivity <- readMVar localThreadStorage
        let messageRecieved = fst userActivity
        let messageSent = snd userActivity
        let updatedUser = User (username user) (messagesSent user + boolToInt messageSent) (messagesRecieved user + boolToInt messageRecieved)
        
        -- | sleep for a random amount of time
        threadDelay delay
        
        -- | restart the thread
        userProcess updatedUser letterBox messageList
        else
            putStrLn $ show user

-- | wrapper function that handles ruterning thread ID and send/receive bool and also kills the thread after the main thread has slept for a bit
-- TODO: research how to kill a thread.

-- | checks the shared memory if a message was sent to the user, then updates the list of all messages
receiveMessage :: User -> MVar Message -> MVar [Message] -> MVar (Bool,Bool) -> IO ()
receiveMessage user letterBox messageList localThreadStorage = do
    currentMessageRead <- readMVar letterBox
    localStorage <- readMVar localThreadStorage
    if target currentMessageRead == username user then do
        currentMessage <- takeMVar letterBox
        updateList <- takeMVar messageList
        let signedMessage = Message (content currentMessage) (sender currentMessage) (target currentMessage) user
        putStrLn $ show signedMessage
        putMVar messageList $ updateList ++ [signedMessage]
        _ <- takeMVar localThreadStorage
        putMVar localThreadStorage (True, snd localStorage)
        else
            return ()


-- | manipulate shared memory to send message
sendMessage :: User -> MVar Message -> MVar (Bool,Bool) -> IO ()
sendMessage user letterBox localThreadStorage = do
    sendDecision <- randomIO :: IO Bool
    localStorage <- readMVar localThreadStorage
    if sendDecision then do
        targetUser <- chooseName (username user) names
        putMVar letterBox $ Message ("Hello "++targetUser++"!") user targetUser dummyUser
        _ <- takeMVar localThreadStorage
        putMVar localThreadStorage (fst localStorage, True)
        else
            return ()


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
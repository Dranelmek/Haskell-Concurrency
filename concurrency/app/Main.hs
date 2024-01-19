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
    userList <- newMVar []
    generateUsers names userList letterBox messageList
    endProcess messageList
    putStrLn "\n================================================\n"
    list <- readMVar messageList
    uList <- readMVar userList
    let finalUserList = countAllUserMessages uList list
    printList finalUserList
    totalSent <- addMessages True finalUserList
    totalReceived <- addMessages False finalUserList
    putStrLn "\n================================================\n"
    let out = "Total messages sent: "++show totalSent++"\nTotal messeages received: "++show totalReceived
    putStrLn out

-- |The list of name used for creating users and choosing message targets
names :: [String]
names = ["Kem","Davide","Francesco","Hamza","Cody","Melike","Sarah","Daisy","Linda","Elizabeth"]

-- |A placeholder variable for the creation of an empty user.
dummyUser :: User
dummyUser = User "" 0 0

-- |The 'userProcess' function creates a thread to simulate a users activity.
userProcess :: User -> MVar Message -> MVar [Message] -> IO ()
userProcess user letterBox messageList = do
    -- | Initialise random variables
    delay <- randomRIO (0,1000) :: IO Int

    threadDelay 100

    -- | Check if the max number of messages has been sent.
    allMessages <- readMVar messageList
    if length allMessages < 100 then do
        -- | Check if the user received a message.
        rTid <- forkIO (receiveMessage user letterBox messageList)
        
        -- | Choose whether or not to send a message to a random user.
        sTid <- forkIO (sendMessage user letterBox)
        
        -- | Sleep the superthread while the subthreads work then kill both subthreads
        threadDelay 30000
        killThread rTid
        killThread sTid

        -- | Sleep for a random amount of time.
        threadDelay delay
        
        -- | Restart the thread.
        userProcess user letterBox messageList
        else do
            return ()

-- |The 'receiveMessage' function checks the shared memory if a message was sent to the user, 
-- then updates the list of all messages.
receiveMessage :: User -> MVar Message -> MVar [Message] -> IO ()
receiveMessage user letterBox messageList = do
    currentMessageRead <- readMVar letterBox
    if target currentMessageRead == username user then do
        currentMessage <- takeMVar letterBox
        updateList <- takeMVar messageList
        let signedMessage = Message (content currentMessage) (sender currentMessage) (target currentMessage) user
        putStrLn $ show signedMessage
        putMVar messageList $ updateList ++ [signedMessage]
        else
            return ()


-- |The 'sendMessage' function can manipulate the shared memory to send a message to a random other user.
-- It has a 50% chance of not sending anything
sendMessage :: User -> MVar Message -> IO ()
sendMessage user letterBox = do
    sendDecision <- randomIO :: IO Bool
    if sendDecision then do
        targetUser <- randomStringFromList (username user) names
        message <- randomStringFromList "" casualMessages
        putMVar letterBox $ Message message user targetUser dummyUser
        else
            return ()

-- |The 'generateUsers' function creates a list of User objects from a list of names and store it in local thread storage.
generateUsers :: [String] -> MVar [User] -> MVar Message -> MVar [Message] -> IO ()
generateUsers [] _ _ _ = return ()
generateUsers (x:xs) userList letterBox messageList= do
    list <- takeMVar userList
    putMVar userList (list++[User x 0 0])
    _ <- forkIO (userProcess (User x 0 0) letterBox messageList)
    generateUsers xs userList letterBox messageList

-- |The 'randomStringFromList' function Chooses a random name from the given list that is not the name given as an argument.
randomStringFromList :: String -> [String] -> IO String
randomStringFromList _ [] =  return ""
randomStringFromList name list = do 
    let bound = length list - 1
    r <- randomRIO (0, bound) :: IO Int
    let out = list!!r
    if out /= name then
        return out
        else
            randomStringFromList name list

-- |The 'printList' function prints every value in a given list.
printList :: Show a => [a] -> IO () 
printList [] = do return ()
printList (x:xs) = do
    putStrLn $ show x
    printList xs

-- |The 'endProcess' ends the main function when the messagelist reaches length 100.
endProcess :: MVar [Message] -> IO ()
endProcess messageList = do
    allMessages <- readMVar messageList
    if length allMessages < 100 then do
        threadDelay 100
        endProcess messageList
        else do
            threadDelay 100
            return ()

-- |Sub function to count all messages sent by a user.
countUserMessages :: User -> [Message] -> (Int,Int)
countUserMessages _ [] = (0,0)
countUserMessages user (x:xs) = (received + fst (countUserMessages user xs), sent + snd (countUserMessages user xs)) where
    received = if recipient x == user then 1 else 0
    sent = if sender x == user then 1 else 0

-- |Centralised function to count all messages sent and received by all users to avoid the threaddelay-miscount error.
countAllUserMessages :: [User] -> [Message] -> [User]
countAllUserMessages [] _ = []
countAllUserMessages (x:xs) messageList = User (username x) sent received : countAllUserMessages xs messageList where
    (received,sent) = countUserMessages x messageList

-- |Test function to check if all sent messages add up to 100.
-- It returns total messages sent when given True and total messages received when given False.
addMessages :: Bool -> [User] -> IO Int
addMessages _ [] = return (0)
addMessages bool (x:[]) = do
    return (func x) where func = if bool then messagesSent else messagesRecieved
addMessages bool (x:xs) = do
    prev <- addMessages bool xs
    return (prev + func x) where func = if bool then messagesSent else messagesRecieved

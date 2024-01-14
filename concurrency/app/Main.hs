{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}

module Main (main) where

import Lib
import Control.Concurrent
import System.Random
import Data.List (elemIndex)

main :: IO ()
main = do 
    letterBox <- newEmptyMVar
    messageList <- newMVar []
    userList <- newMVar []
    generateUsers names userList letterBox messageList
    endProcess messageList
    putStrLn "\n================================================\n"
    finalList <- readMVar userList
    printList finalList
    -- ml <- readMVar messageList
    -- putStrLn $ show $ length ml



{-| TODO: solve self overtaking problem
    Possible solution: find optimal sleep times
    Add random messages
    -}

names = ["James","Robert","John","Michael","David","Mary","Patricia","Jennifer","Linda","Elizabeth"]
dummyUser = User "" 0 0

-- | create user thread
userProcess :: User -> MVar [User] -> MVar Message -> MVar [Message] -> IO ()
userProcess user userList letterBox messageList = do
    -- | initialise random variables
    delay <- randomRIO (0,1000) :: IO Int
    localThreadStorage <- newMVar (False,False)

    threadDelay 100

    -- | check if the max number of messages has been sent
    allMessages <- readMVar messageList
    if length allMessages < 100 then do
        -- | check if the user received a message
        rTid <- forkIO (receiveMessage user letterBox messageList localThreadStorage)
        
        -- | send a message to a random user
        sTid <- forkIO (sendMessage user letterBox localThreadStorage)
        
        -- | sleep the superthread while the subthreads work then kill both subthreads
        threadDelay 50000
        killThread rTid
        killThread sTid

        -- | update the user's fields
        userActivity <- readMVar localThreadStorage
        let messageRecieved = fst userActivity
        let messageSent = snd userActivity
        let updatedUser = User (username user) (messagesSent user + boolToInt messageSent) (messagesRecieved user + boolToInt messageRecieved)
        
        -- | update the userlist
        updateUserList updatedUser userList

        -- | sleep for a random amount of time
        threadDelay delay
        
        -- | restart the thread
        userProcess updatedUser userList letterBox messageList
        else do
            return ()

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


generateUsers :: [String] -> MVar [User] -> MVar Message -> MVar [Message] -> IO ()
generateUsers [] _ _ _ = return ()
generateUsers (x:xs) userList letterBox messageList= do
    list <- takeMVar userList
    putMVar userList (list++[User x 0 0])
    _ <- forkIO (userProcess (User x 0 0) userList letterBox messageList)
    generateUsers xs userList letterBox messageList

-- | chooses a random name from the given list that is not the name given as an argument
chooseName :: String -> [String] -> IO String
chooseName _ [] =  return ""
chooseName name list = do 
    r <- randomRIO (0,9) :: IO Int
    let out = list!!r
    if out /= name then
        return out
        else
            chooseName name list

-- | find a user in a list of users and replace it with the new one
replaceUser :: User -> [User] -> [User]
replaceUser user userList = 
    x ++ user : ys where
        (x,_:ys) = splitAt ind userList where
            ind = getMaybeInt $ elemIndex user userList

-- | get int or -1 out of Maybe Int
getMaybeInt :: Maybe Int -> Int
getMaybeInt Nothing = -1
getMaybeInt (Just a) = a

-- | update the userlist
updateUserList :: User -> MVar [User] -> IO ()
updateUserList user userList = do
    list <- takeMVar userList
    putMVar userList $ replaceUser user list


boolToInt :: Bool -> Int
boolToInt a | a = 1 | otherwise = 0

printList :: Show a => [a] -> IO () 
printList [] = do return ()
printList (x:xs) = do
    putStrLn $ show x
    printList xs

-- | end the main function when 
endProcess :: MVar [Message] -> IO ()
endProcess messageList = do
    allMessages <- readMVar messageList
    if length allMessages < 100 then do
        threadDelay 100
        endProcess messageList
        else do
            threadDelay 10000
            return ()

{-
Things I found:
challenges: 
The system could deadlock in various situations:
- at the start if there is no sent message in the system and all threads try to read a message
- if there is a message to a certain thread and that thread is trying to send a message.
Solution:
- make the threads only wait for a certain time before moving on with their life
Delay and cutoff issue: 
- after 100 messages have been sent the main thread seems to end the entire process 
- this leads to not all user threads displaying their final information
Solution:
- make the end process function wait longer
- centralise the action of printing information into the main function rather than having each thread print their information individually
self skipping issue:
- The system doesn't display all messages sent/received after the simulation
- I assume there are timing issues somewhere as the execution is sometimes stopped by indefinite MVar wait
Solution:
- good old fashion debugging

the prototype is done but the next step is figuring out how to beat the afromentioned deadlock.
the deadlock can be solved by spawning 2 subthreads for each user thread

-}
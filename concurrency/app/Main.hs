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



{-| TODO: solve self overtaking problem
    Possible solution: find optimal sleep times
    Add random messages
    I will probably have a long list of strings in lib.hs that will contain a bunch of predefined messages
    another way would be to api fetch for an open source chat bot
    -}

names :: [String]
names = ["Kem","Davide","Francesco","Hamza","Cody","Melike","Sarah","Daisy","Linda","Elizabeth"]

-- | placeholder variable for the creation of an empty user
dummyUser :: User
dummyUser = User "" 0 0

-- | create user thread
userProcess :: User -> MVar Message -> MVar [Message] -> IO ()
userProcess user letterBox messageList = do
    -- | initialise random variables
    delay <- randomRIO (0,1000) :: IO Int

    threadDelay 100

    -- | check if the max number of messages has been sent
    allMessages <- readMVar messageList
    if length allMessages < 100 then do
        -- | check if the user received a message
        rTid <- forkIO (receiveMessage user letterBox messageList)
        
        -- | send a message to a random user
        sTid <- forkIO (sendMessage user letterBox)
        -- | sleep the superthread while the subthreads work then kill both subthreads
        threadDelay 30000

        killThread rTid
        killThread sTid

        -- | sleep for a random amount of time
        threadDelay delay
        
        -- | restart the thread
        userProcess user letterBox messageList
        else do
            return ()

-- | checks the shared memory if a message was sent to the user, then updates the list of all messages
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


-- | manipulate shared memory to send message
sendMessage :: User -> MVar Message -> IO ()
sendMessage user letterBox = do
    sendDecision <- randomIO :: IO Bool
    if sendDecision then do
        targetUser <- randomStringFromList (username user) names
        message <- randomStringFromList "" casualMessages
        putMVar letterBox $ Message message user targetUser dummyUser
        else
            return ()


generateUsers :: [String] -> MVar [User] -> MVar Message -> MVar [Message] -> IO ()
generateUsers [] _ _ _ = return ()
generateUsers (x:xs) userList letterBox messageList= do
    list <- takeMVar userList
    putMVar userList (list++[User x 0 0])
    _ <- forkIO (userProcess (User x 0 0) letterBox messageList)
    generateUsers xs userList letterBox messageList

-- | chooses a random name from the given list that is not the name given as an argument
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

printList :: Show a => [a] -> IO () 
printList [] = do return ()
printList (x:xs) = do
    putStrLn $ show x
    printList xs


-- | end the main function when the messagelist reaches length 100
endProcess :: MVar [Message] -> IO ()
endProcess messageList = do
    allMessages <- readMVar messageList
    if length allMessages < 100 then do
        threadDelay 100
        endProcess messageList
        else do
            threadDelay 100
            return ()

-- | test function to check if all sent messages add up to 100
-- | It returns total messages sent when given True and total messages received when given False
addMessages :: Bool -> [User] -> IO Int
addMessages _ [] = return (0)
addMessages bool (x:[]) = do
    return (func x) where func = if bool then messagesSent else messagesRecieved
addMessages bool (x:xs) = do
    prev <- addMessages bool xs
    return (prev + func x) where func = if bool then messagesSent else messagesRecieved



-- | sub function to count all messages sent by a user
countUserMessages :: User -> [Message] -> (Int,Int)
countUserMessages _ [] = (0,0)
countUserMessages user (x:xs) = (received + fst (countUserMessages user xs), sent + snd (countUserMessages user xs)) where
    received = if recipient x == user then 1 else 0
    sent = if sender x == user then 1 else 0

-- | centralised function to count all messages sent and received by all users to avoid the threaddelay-miscount error
countAllUserMessages :: [User] -> [Message] -> [User]
countAllUserMessages [] _ = []
countAllUserMessages (x:xs) messageList = User (username x) sent received : countAllUserMessages xs messageList where
    (received,sent) = countUserMessages x messageList

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
- another more experimental approach would be to perform the counting of messages per user centrally in the main function
  rather than in the process for each user. the time complexity for this would be an additional n^2 however it would
  eliminate the inconsistency and the specification does not mention efficiency.

the prototype is done but the next step is figuring out how to beat the afromentioned deadlock.
the deadlock can be solved by spawning 2 subthreads for each user thread

-}
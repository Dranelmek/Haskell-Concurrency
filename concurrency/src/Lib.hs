module Lib
    ( User
    , Message
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Name = String

data User = User {
    username :: Name,
    messagesSent :: Int,
    messagesRecieved :: Int
}

data Message = Message {
    content :: String,
    sender :: User,
    recipient :: User
}
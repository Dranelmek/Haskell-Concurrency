module Lib
    ( User(..)
    , Message(..)
    ) where

data User = User 
    { username :: String
    , messagesSent :: Int
    , messagesRecieved :: Int
    }

instance Eq User where
    a == b = username a == username b

instance Show User where
    show a = username a


data Message = Message {
    content :: String,
    sender :: User,
    recipient :: User
} deriving Eq

instance Show Message where
    show a = (show $ sender a) ++ " sent " ++ (show $ recipient a) ++ " a message saying:\n" ++ content a
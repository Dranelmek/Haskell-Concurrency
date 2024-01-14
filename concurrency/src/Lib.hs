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
    show a = "User: " ++ username a ++ "\nMessages sent: " ++ (show $ messagesSent a) ++ "\nMessages recieved: " ++ (show $ messagesRecieved a)


data Message = Message {
    content :: String,
    sender :: User,
    target :: String,
    recipient :: User
} deriving Eq

instance Show Message where
    show a = (username $ sender a) ++ " sent " ++ (username $ recipient a) ++ " a message saying:\n" ++ content a

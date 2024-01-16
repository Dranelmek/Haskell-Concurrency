{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Lib
    ( User(..)
    , Message(..)
    , casualMessages
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

casualMessages = [
    "Hey, how are you?",
    "What's going on?",
    "Long time no see!",
    "How's your day so far?",
    "Any plans for the weekend?",
    "Did you see the latest movie?",
    "I can't believe it's already Friday!",
    "How about that weather?",
    "Just wanted to drop in and say hi!",
    "Thinking of you!",
    "How's life treating you these days?",
    "Any juicy gossip?",
    "Can you believe this traffic?",
    "Let's grab coffee sometime.",
    "I'm so tired today.",
    "What's your go-to comfort food?",
    "Had the weirdest dream last night.",
    "Got any weekend plans?",
    "I'm craving pizza.",
    "Any good jokes you've heard lately?",
    "Stuck in traffic. Ugh!",
    "Let's plan something fun for the weekend!",
    "What's your favorite book?",
    "I'm feeling lazy today.",
    "Guess who I ran into today?",
    "Let's have a movie night soon.",
    "Any exciting news?",
    "Proud of you for that accomplishment!",
    "Let's reminisce about old times.",
    "Can't wait for the weekend!",
    "I'm obsessed with this song.",
    "Any good TV shows you're watching?",
    "Let's plan a spontaneous road trip!",
    "I'm feeling inspired today.",
    "Best way to relax after a long day?",
    "Any Netflix recommendations?",
    "Let's catch up soon!",
    "I'm so over Among us.",
    "I'm on vacation mode!",
    "How's your pet doing?",
    "Need a break from work.",
    "Favorite childhood memory?",
    "I need some book recommendations.",
    "Let's plan a game night!",
    "I'm so glad we're friends!",
    "Best piece of advice you've received?",
    "How do you stay positive during tough times?",
    "Excited for the coursework subimssion?",
    "I'm on a functional programming kick.",
    "I need a good laugh.",
    "Any new podcasts to listen to?",
    "Let's go on an adventure!",
    "I'm so bad at writing essays.",
    "Trying to learn pixil art. Tips?",
    "What's your guilty pleasure TV show?",
    "Let's plan a picnic in the park!",
    "Feeling nostalgic today.",
    "I can't believe it's already January.",
    "I'm so ready for semester B.",
    "Let's plan a weekend brunch!",
    "Any recent achievements you're proud of?",
    "Favorite way to spend a lazy Sunday?",
    "I'm on a quest to find the perfect meme.",
    "I need some motivation today.",
    "Let's have a virtual hangout!",
    "I'm on a mission to find the best coffee shop.",
    "What's the last concert you attended?",
    "Just finished a great workout.",
    "Excited for school!",
    "Let's plan a movie marathon!",
    "Feeling blessed today.",
    "Favorite genre of music?",
    "I just started a new hobby.",
    "I'm so proud of you for reading this Edmund!",
    "Favorite type of snack?",
    "I just had the best coffee ever.",
    "I'm in the mood for some classical music.",
    "What's the last thing that made you smile?",
    "I'm on a mission to find the best burgers in town.",
    "Just learned something new today.",
    "Excited about moving?",
    "Favorite thing to do on a lazy day?",
    "Feeling inspired today.",
    "Let's plan a virtual game night!",
    "I'm on a mission to find the perfect module.",
    "What's the last thing you treated yourself to?",
    "Just discovered this amazing TV show.",
    "So glad we're friends!",
    "Favorite way to spend a Saturday night?",
    "In the mood for some comfort food.",
    "Just had the best dessert ever.",
    "I'm impressed by your talent/skill.",
    "Most interesting thing you've learned recently?",
    "On a mission to find the perfect book.",
    "Just realized I've never tried parachuting.",
    "So ready for uni to start.",
    "Last thing that made you happy?"
    ]

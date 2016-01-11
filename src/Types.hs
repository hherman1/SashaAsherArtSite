{-# LANGUAGE DeriveGeneric #-}

module Types where
import Data.Aeson
import Data.Time.Clock
import Data.ByteString (ByteString)
import GHC.Generics

type ID = Int
type SID = Int
type UID = Int
type Token = String

data Plaintext = Plaintext String String
    deriving (Ord, Eq, Show, Generic)

data SessionToken = 
    SessionToken {
        sessionId :: ID, 
        sessionToken :: Token
        }
    deriving (Ord, Eq, Show, Generic)


data AppError = UserExists | InvalidCredentials
    deriving (Eq,Show)

data Medium = Paper | Canvas | Stone
    deriving (Eq, Show, Generic)

type Password = ByteString

data User = User 
    { user_id :: Int 
    , user_alias :: String
    , user_pass :: String
    , user_name :: String
    , user_email :: String
    , user_phone :: Maybe String
    , user_address :: Maybe String
    , user_registration_time :: UTCTime
    }
    deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

data Item = Item 
    { item_id :: Int
    , item_author :: Int
    , item_owner :: Int
    , item_title :: String
    , item_description :: String
    , item_price :: String
    , item_date :: UTCTime
    , item_sold :: Bool
    , item_quantity :: Int
    , item_medium :: Medium
    , item_tags :: [String]
    }
    deriving (Eq, Show, Generic)

data Liked = Liked {like_id :: Int, like_owner :: Int, like_post :: Int}
    deriving (Eq, Show, Generic)

data Follow = Follow {follow_id :: Int, follow_owner :: Int, follow_target :: Int}
    deriving (Eq, Show, Generic)

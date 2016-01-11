{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Data.Aeson
import Data.Time.Clock
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Types
import Auth


type API = RequestSession :<|> UserApi :<|> ItemApi :<|> LikeApi :<|> FollowApi



type AuthApi = "auth" :> SessionApi

type SessionApi = "session" :> (RequestSession :<|> EndSession)

type RequestSession = Header "Authorization" Plaintext :> "get" :> Get '[JSON] (Headers '[Header "Set-Cookie" (SessionToken,UTCTime)] ())

type EndSession = WithAuth :> "delete" :> Get '[JSON] (Headers '[Header "Set-Cookie" (SessionToken,UTCTime)] ())


type UserApi = "users" :> (CreateUser :<|> GetUser :<|> DeleteUser)

type CreateUser = "create" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe String)

type GetUser = WithAuth :> "get" :> Capture "uid" UID :> Get '[JSON] (Maybe User)

type DeleteUser = WithAuth :> "delete" :> Delete '[JSON] ()




type ItemApi = CreateItem :<|> GetItem :<|> DeleteItem

type CreateItem = WithAuth :> "items" :> "create" :> QueryParam "item" Item :> Post '[JSON] (Maybe String)

type GetItem = WithAuth :> "items" :> "get" :> Capture "itemid" Int :> Get '[JSON] Item

type DeleteItem = WithAuth :> "items" :> "delete" :> Capture "itemid" Int :> Delete '[JSON] (Maybe String)




type LikeApi = CreateLike :<|> DeleteLike

type CreateLike = WithAuth :> "items" :> "like" :> QueryParam "like" :> Post '[JSON] (Maybe String)

type DeleteLike = WithAuth :> "items" :> "unlike" :> Capture "likeid" Int :> Delete '[JSON] (Maybe String)


type FollowApi = CreateFollow :<|> DeleteFollow

type CreateFollow = WithAuth :> "users" :> "follow" :> QueryParam "follow" Follow :> Post '[JSON] (Maybe String)

type DeleteFollow = WithAuth :> "users" :> "unfollow" :> Capture "followid" Int :> Delete '[JSON] (Maybe String)
	:<|> "users" :> "unfollow" :> Capture "followerid" Int :> Capture "userid" Int :> Delete '[JSON] (Maybe String)


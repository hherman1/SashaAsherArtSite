{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Aeson
import Data.Maybe
import Data.Time.Clock
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import GHC.Generics
import Network.Wai 
import Network.Wai.Handler.Warp hiding (Connection)
import Database.HDBC.Sqlite3
import Servant
import API
import Sql
import Auth
import Types
import Test


--instance ToJSON Day where
--  toJSON d = toJSON (showGregorian d)

type Serv t = EitherT ServantErr IO t

type TestApi = RequestSession :<|> UserApi

exec :: IO ()
exec = do
    c <- conn
    run 8081 (app c)

recompiler = show "test"

app :: Connection -> Application
app c = serve proxy (api c)

proxy :: Proxy TestApi
proxy = Proxy

api :: Connection -> Server (TestApi)
api c = (requestSession c) :<|> (userApi c)

requestSession :: Connection -> Maybe Plaintext -> Serv (Headers '[Header "Set-Cookie" (SessionToken,UTCTime)] ())
requestSession c creds = (maybe (left err401) return) =<< (runMaybeT $ do
    cred <- MaybeT . return $ creds
    token <- MaybeT $ lift (loginSession c cred)
    return $ addHeader token ())

userApi :: Connection -> Server UserApi
userApi c = createUser 
    :<|> getUser
    :<|> deleteUser
    where
        createUser :: User -> Serv (Maybe String) 
        createUser u = do
            res <- lift $ Auth.createUser c u
            case res of
                Left err -> return $ Just $ show err
                Right _ -> return $ Nothing

        getUser :: SID -> UID -> UID -> Serv (Maybe User)
        getUser _ _ uid = lift $ Sql.getUser c uid
        
        deleteUser :: SID -> UID -> Serv ()
        deleteUser _ uid = lift $ Sql.deleteUser c uid

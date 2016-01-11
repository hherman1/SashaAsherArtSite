module Sql.Sessions where

import Data.List (intersperse)
import Data.Time.Clock
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

import Types
import Sql.Utils


newSession :: Connection -> UID -> Token -> UTCTime -> IO (Maybe SessionToken)
newSession c uid token time = do
    run c "INSERT INTO \
        \sessions (uid,token,expiration_date) \
        \VALUES (?,?,?)"
        [toSql uid, toSql token, toSql time]
    runMaybeT $ do
        sid <- MaybeT $ lastRowId c
        lift $ commit c
        return $ SessionToken sid token
    
updateSession :: Connection -> ID -> String -> UTCTime -> IO ()
updateSession c sid token expiration = do
    run c "UPDATE sessions \
        \SET token=?,expiration_date=? \
        \WHERE id=?"
        [toSql token,toSql expiration,toSql sid]
    commit c

openSessions :: Connection -> UID -> IO [(ID,UTCTime)]
openSessions c uid = do
    res <- quickQuery c "SELECT id, expiration_date \
        \FROM sessions \
        \WHERE uid=?"
        [toSql uid]
    return $ map (\[sid,time] -> (fromSql sid,fromSql time)) res

deleteSessions :: Connection -> [ID] -> IO (Maybe ())
deleteSessions c sids = runMaybeT $ do
    guard . not . null $ sids
    s <- return $   "DELETE FROM sessions WHERE " 
            ++ (concat $ 
            intersperse "OR" [" id=? " | sid <- sids])
    lift $ run c s (map toSql sids)
    lift $ commit c

getUIDAndExpiration :: Connection -> SessionToken -> IO (Maybe (UID,UTCTime))
getUIDAndExpiration c (SessionToken sid token) = do
    res <- fetch c "SELECT uid,expiration_date \
        \FROM sessions \
        \WHERE id=? AND token=?" 
        [toSql sid,toSql token]
    return $ fmap (\[uid,t] -> (fromSql uid, fromSql t)) res


getExpiration :: Connection -> SessionToken -> IO (Maybe UTCTime)
getExpiration c (SessionToken sid token) = do
    res <- fetch c "SELECT expiration_date \
            \FROM sessions \
            \WHERE id=? AND token=?" 
            [toSql sid, toSql token]
    case res of
        Just [time] -> return $ Just (fromSql time)
        _ -> return Nothing

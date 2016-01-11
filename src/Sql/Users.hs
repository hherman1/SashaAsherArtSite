module Sql.Users where
import Data.ByteString 

import Database.HDBC
import Database.HDBC.Sqlite3

import Types
import Sql.Utils

-- CAN ERROR
insertUser :: Connection -> User -> Password -> IO ()
insertUser c (User _ un _ n e ph a r) p = do 
    run c "INSERT INTO \
        \users (uname,pass,name,email,phone,address,registration_date)\
        \VALUES (?,?,?,?,?,?,?)"
        [toSql un, toSql p, toSql n, toSql e, toSql ph, toSql a, toSql r]
    commit c

getUIDAndPass :: Connection -> String -> IO (Maybe (UID,ByteString))
getUIDAndPass c uname = do
    res <- fetch c "SELECT id, pass \
            \FROM users \
            \WHERE uname=?" 
            [toSql uname]
    return $ fmap (\[uid,pass] -> (fromSql uid, fromSql pass)) res
        

getUser :: Connection -> UID -> IO (Maybe User)
getUser c uid = do
    res <- fetch c "SELECT id,\
            \uname,\
            \name,\
            \email,\
            \phone,\
            \address,\
            \registration_date \
            \FROM users \
            \WHERE id=?" 
            [toSql uid]
    case res of
        Just [u,un,n,e,ph,ad,rd] -> 
            return $ Just $ User 
                (fromSql u) 
                (fromSql un) 
                "" 
                (fromSql n) 
                (fromSql e) 
                (fromSql ph) 
                (fromSql ad) 
                (fromSql rd)
        _ -> return Nothing

deleteUser :: Connection -> UID -> IO ()
deleteUser c uid = do
    run c "DELETE FROM users WHERE id=?" [toSql uid]
    run c "DELETE FROM sessions WHERE uid=?" [toSql uid]
    commit c





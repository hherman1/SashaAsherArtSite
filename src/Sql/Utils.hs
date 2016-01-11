module Sql.Utils where
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Maybe
import Types

conn :: IO Connection
conn = connectSqlite3 "hunter.db" 

fetch :: Connection -> String -> [SqlValue] -> IO (Maybe [SqlValue])
fetch c s vs = do
    com <- prepare c s
    execute com vs
    fetchRow com

lastRowId :: Connection -> IO (Maybe Int)
lastRowId c = do
    res <- fetch c "SELECT last_insert_rowid()" []
    return $ fromSql <$> (listToMaybe =<< res)

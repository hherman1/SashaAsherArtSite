module Sql.Init where
import Database.HDBC
import Database.HDBC.Sqlite3

initUsers :: Connection -> IO ()
initUsers c = do 
    run c "CREATE TABLE users (\
        \id INTEGER NOT NULL UNIQUE PRIMARY KEY,\
        \uname TEXT UNIQUE NOT NULL,\
        \pass TEXT NOT NULL,\
        \name TEXT,\
        \email TEXT NOT NULL,\
        \phone TEXT,\
        \address TEXT,\
        \registration_date TEXT)" []
    commit c

initSessions :: Connection -> IO ()
initSessions c = do
    run c "CREATE TABLE sessions (\
        \id INTEGER NOT NULL UNIQUE PRIMARY KEY,\
        \uid INTEGER NOT NULL,\
        \token TEXT NOT NULL,\
        \expiration_date TEXT NOT NULL,\
        \FOREIGN KEY(uid) REFERENCES users(id))" []
    commit c

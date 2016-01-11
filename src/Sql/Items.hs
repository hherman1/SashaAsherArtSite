module Sql.Items where
import Database.HDBC.Sqlite3
import Sql.Utils

insertItem :: Connection -> Item -> IO ()
insertItem (Item 
    id
    uid
    title
    desc
    price
    date
    sold
    quantity
    medium
    tags) 
    = do
    run c 
        "INSERT INTO items \
        \(uid\
        \,title\
        \,description\
        \,price\
        \,post_date\
        \,sold\
        \,quantity\
        \,medium\
        \,tags\
        \) VALUES \
        \(?,?,?,?,?,?,?,?,?)"
        [toSql id
        ,toSql uid
        ,toSql title
        ,toSql desc
        ,toSql price
        ,toSql date
        ,toSql sold
        ,toSql quantity
        ,toSql medium
        ,toSql tags
        ]
    commit c

deleteItem :: Connection -> ID -> IO ()
deleteItem c pid =
    run c
        "DELETE FROM items \
        \WHERE id=?"
        [toSql pid]
    commit c

getItem :: Connection -> ID -> IO Item
getItem c pid =
    res <- fetch c 
        "SELECT * FROM items \
        \WHERE id=?"
        [toSql pid]
    return $ fmap 
        (\[id
        ,uid
        ,title
        ,desc
        ,price
        ,date
        ,sold
        ,quantity
        ,medium
        ,tags
        ] -> Item
        (fromSql id)
        (fromSql uid)
        (fromSql title)
        (fromSql desc)
        (fromSql price)
        (fromSql date)
        (fromSql sold)
        (fromSql quantity)
        (fromSql medium)
        (fromSql tags)

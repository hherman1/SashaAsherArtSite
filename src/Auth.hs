{-
 - Copyright 2016 Hunter Herman 
 - (hherman1@macalester.edu)
 - Authentication/Session management 
 - for "Artstagram" (working name)
 -
 -}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Auth 
    where
import              Data.Aeson
import              Data.Text   
                        (Text,append
                        ,pack
                        ,stripPrefix
                        ,unpack
                        ,splitOn)
import              Data.Text.Encoding
import              Data.Maybe
import              Data.Time.Format
import              Data.Time.Clock
import              Control.Monad
import              Control.Monad.Trans.Class
import              Control.Monad.Trans.Maybe
import              Crypto.BCrypt
import              System.Entropy
import              Database.HDBC
import              Database.HDBC.Sqlite3
import              Network.HTTP.Types
import              Network.Wai
import              Servant
import              Servant.Server.Internal
import              Web.Cookie
import              Sql
import              Types
import              Data.ByteString 
                        (ByteString)
import  qualified   Data.ByteString.Base64 
        as          ENC
import  qualified   Data.ByteString.Char8 
        as          BS
import              Data.ByteString.Conversion.To 
                        (ToByteString
                        ,builder)

tokenDuration = fromInteger $ 60 * 60
refreshRange = fromInteger $ 60 * 15
entropyBytes = 64

newExpirationDate :: IO UTCTime
newExpirationDate = 
    addUTCTime 
        tokenDuration 
        <$> getCurrentTime

hoistMaybe :: (Monad m) => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return

instance ToByteString (SessionToken,UTCTime) where
    builder (token,time) = renderSetCookie $ setCookie token time

instance FromText Plaintext where 
    fromText t = case unBasic t of
        Just [uname,pass] -> do
            let u = unpack uname
                p = unpack pass
            return $ Plaintext u p
        _ -> Nothing

instance FromText SessionToken where
    fromText t = case unBasic t of
        Just [sid,token] -> do
            let t = unpack token
            s <- fromText sid
            return $ SessionToken s t
        _ -> Nothing

setCookie :: SessionToken -> UTCTime -> SetCookie
setCookie token time = 
    def { setCookieName     = "session"
        , setCookieValue    = basicSession token
        , setCookieExpires  = Just time
        , setCookieHttpOnly = True
        , setCookieSecure   = True
        }

fullSessCookie :: SessionToken -> UTCTime -> BS.ByteString
fullSessCookie token time = 
    BS.append
        (sessionCookie token) 
        $ BS.append
            (encodeUtf8 "; ") 
            (expirationByteString time)

expirationByteString :: UTCTime -> BS.ByteString
expirationByteString = 
    encodeUtf8 
    . pack 
    . formatHTTP

sessionCookie :: SessionToken -> BS.ByteString
sessionCookie = 
    BS.append 
        (encodeUtf8 "session=") 
        . basicSession

basicSession :: SessionToken -> BS.ByteString
basicSession (SessionToken sid token) = 
    toBasic 
    . pack 
    $   show sid 
        ++ ":" 
        ++ token

toBasic :: Text -> BS.ByteString
toBasic t = 
    BS.append 
        (encodeUtf8 "Basic ") 
        (toBase64 t)

toBase64 :: Text -> BS.ByteString
toBase64 = 
    ENC.encode 
    . encodeUtf8 
    

unBasic :: Text -> Maybe [Text]
unBasic ( stripPrefix "Basic " -> Just enc) = 
    either 
        (const Nothing) 
        Just
            . splitOn ":" 
            . decodeUtf8 
            <$> ENC.decode 
            (encodeUtf8 enc) 
unBasic _ = Nothing


genToken :: IO Token
genToken = 
    BS.unpack 
        <$> getEntropy entropyBytes

hash :: String -> IO ByteString
hash s = 
    fromMaybe 
        (error 
            $ "Failed to hash string " 
            ++ show s)
        <$> hashPasswordUsingPolicy 
            fastBcryptHashingPolicy 
            (BS.pack s)

createUser :: Connection -> User -> IO (Either AppError ())
createUser c u = do
    h <- hash $ user_pass u
    catchSql 
        (Right <$> insertUser c u h) 
        (const . return $ Left UserExists)


validate :: Connection -> Plaintext -> IO (Maybe UID)
validate c (Plaintext uname pass) = runMaybeT $ do
    (uid,valid) <- 
        MaybeT 
        $ getUIDAndPass c uname
    if validatePassword valid (BS.pack pass) 
        then return uid
        else hoistMaybe Nothing

validateSession :: 
    Connection 
    -> SessionToken 
    -> IO (Maybe (UID,UTCTime))
validateSession = getUIDAndExpiration
        
cleanSessions :: Connection -> UID -> IO (Maybe ())
cleanSessions c uid = do
    sess <- openSessions c uid
    t <- getCurrentTime
    let sids = 
        fst 
        <$> filter 
            ((t>) . snd) 
            sess
    deleteSessions c sids

newSession :: 
    Connection 
    -> UID 
    -> IO (Maybe (SessionToken,UTCTime))
newSession c uid = do
    t <- newExpirationDate
    token <- genToken
    fmap (flip (,) t) <$> Sql.newSession c uid token t

loginSession :: 
    Connection 
    -> Plaintext 
    -> IO (Maybe (SessionToken,UTCTime))
loginSession c signin = runMaybeT $ do
    uid <- 
        MaybeT 
        $ validate c signin
    MaybeT $ Auth.newSession c uid
        

--Performs no validation on the token
refreshToken :: 
    Connection 
    -> SessionToken 
    -> UTCTime 
    -> IO (SessionToken,UTCTime)
refreshToken c (SessionToken sid token) expiration = do
    t <- getCurrentTime
    if addUTCTime refreshRange t >= expiration then do
        newToken <- genToken
        exp <- newExpirationDate
        Sql.updateSession 
            c 
            sid 
            newToken 
            (addUTCTime tokenDuration exp)
        return 
            (SessionToken sid newToken
            , exp)
    else
        return 
            (SessionToken sid token
            , expiration)

deleteSession :: 
    Connection 
    -> SessionToken 
    -> IO (Maybe ())
deleteSession c sess@(SessionToken sid _) = 
    runMaybeT $ do
        MaybeT $ validateSession c sess
        MaybeT $ deleteSessions c [sid]

data WithAuth

instance HasServer sublayout => HasServer (WithAuth :> sublayout) where
    type ServerT (WithAuth :> sublayout) m = 
        SID 
        -> UID 
        -> ServerT sublayout m
    route Proxy subserver request respond = 
        maybe fail401 return =<<
            runMaybeT 
                (do c <- lift conn
                    token <- hoistMaybe 
                        $   getSessionCookie
                        =<< getCookies 
                            request

                    (uid,expiration) <- 
                        MaybeT 
                        $ getUIDAndExpiration c token

                    lift $ cleanSessions c uid
                    time <- lift getCurrentTime
                    
                --  Checks the token is not expired
                    guard 
                        (time < expiration) 

                    (newToken@(SessionToken sid _), newExp) <- 
                        lift 
                        $ Auth.refreshToken     
                            c 
                            token 
                            expiration 

                    lift $ disconnect c
                    lift $ route    (Proxy :: Proxy sublayout) 
                            (subserver sid uid) 
                            request 
                            (respond 
                                . mapRR 
                                (addHeader 
                                    $ fullSessCookie 
                                        newToken 
                                        newExp)))
        where
            fail401 = respond 
                $ succeedWith 
                $ responseLBS status401 
                    [("WWW-Authenticate", "Basic realm=\"nmrs_m7VKmomQ2YM3:\"")
                    ,("Location","/auth/get/token/")] "" 

            getSessionCookie :: CookiesText -> Maybe SessionToken
            getSessionCookie cookies =
                fromText =<< lookup "session" cookies

            getCookies :: Request -> Maybe CookiesText
            getCookies req = 
                parseCookiesText 
                <$> lookup
                    "Cookie" 
                    (requestHeaders 
                        req)

            addHeader t = mapResponseHeaders 
                (("Set-Cookie",  t ) :)
            mapRR f (RR r) = RR (fmap f r)

module Test where
import Data.Time.Clock
import Types

testToken = SessionToken 1 ""

credentials = Plaintext "test" "abc"

testUser = getCurrentTime >>= (\t -> return $ User 0 "test" "abc" "Hunter Herman" "hherman1@macalester.edu" (Just "6468242227") (Just "301 E. 69th Street") t)

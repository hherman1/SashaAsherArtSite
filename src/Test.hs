module Test where
import Data.Time.Clock
import Types

testToken = SessionToken 1 ""

credentials = Plaintext "test" "abc"

testUser = getCurrentTime >>= (\t -> return $ User 0 "test" "abc" "Test" "test@test.edu" (Just "5555555555") (Just "562 E. 12th Street") t)

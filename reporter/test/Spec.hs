{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

{-
import           Education.MakeMistakesToLearnHaskell.Report.Server (app)

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
-}

main :: IO ()
main = putStrLn "No tests so far! sorry!" -- hspec spec

{-
spec :: Spec
spec = with (return app) $ do
    describe "GET /users" $ do
        it "responds with 200" $ do
            get "/users" `shouldRespondWith` 200
        it "responds with [User]" $ do
            let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"}]"
            get "/users" `shouldRespondWith` users
-}

moves
========

moves - Haskell libray for moves-app.com's API

How to use the library
--------
=== Setup API keys ===
* Register a app on dev.moves-app.com to get the API keys
* mv Moves/ApiKey.hs.sample Moves/ApiKey.Hs -- And add client-id and and client-secret
* cabal install
* MovesCreateAccessToken -- Follow instrunctions to create a access token
* Add the token recivied in previous step in ApiKey.hs
* You now have a AccessToken that can be used for 180 days
* cabal install -- Rebuild the code to use the new AccessToken
* MovesGet "/user/activities/daily?pastDays=3" -- use MovesGet to test the api
* See example program in example/ to see how to use the lib

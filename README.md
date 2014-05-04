moves
========

moves - Haskell libray for moves-app.com's API

Dependencies:
-------
TODO


How to use the library
--------
=== Setup API keys ===
* Register a app on dev.moves-app.com to get the API keys
* mv ApiKey.hs.sample ApiKey.Hs -- And add client-id and and client-secret
* make
* ./CreateAccessToken -- Follow instrunctions to create a access token
* Add the token recivied in previous step in ApiKey.hs
* You now have a AccessToken that can be used for 180 days
* make -- Rebuild the code to use the new AccessToken
* ./MovesGet "/user/activities/daily?pastDays=3" -- use MovesGet to test the api

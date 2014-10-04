module
HTTPHelper
where

import Network.HTTP

-- Get the HTML page in raw
openURL :: String -> IO String
openURL url = getResponseBody =<< simpleHTTP (getRequest url)


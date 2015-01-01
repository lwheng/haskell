module Handler.Echo where

import Import

getEchoR :: String -> Handler Html
getEchoR string = defaultLayout [whamlet|<h1>#{string}|]

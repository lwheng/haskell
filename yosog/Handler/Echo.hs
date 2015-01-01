module Handler.Echo where

import Import

getEchoR :: Text -> Handler Html
-- getEchoR theText = defaultLayout [whamlet|<h1>#{theText}|]
getEchoR theText = defaultLayout $(widgetFile "echo")

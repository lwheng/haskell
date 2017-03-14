{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/      HomeR  GET
/root  RootR  GET
|]

instance Yesod App where

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "toWidgetHead and toWidgetBody"
    toWidgetBody
        [hamlet|<script src=/included-in-body.js>|]
    toWidgetHead
        [hamlet|<script src=/included-in-head.js>|]

getRootR = defaultLayout $ do
  headerClass <- newIdent
  toWidget [hamlet|<h1 .#{headerClass}>My Header|]
  toWidget [lucius| .#{headerClass} { color: green; } |]

main :: IO ()
main = warp 3001 App

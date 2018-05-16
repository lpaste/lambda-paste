{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Monoid
import           Data.Pool
import           Data.Text (Text)
import           Data.Time (getCurrentTime, UTCTime)
import           Database.Persist.Postgresql
import           Yesod

--------------------------------------------------------------------------------
-- App

data App = App (Pool SqlBackend)

instance Yesod App where

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        App pool <- getYesod
        runSqlPool action pool

--------------------------------------------------------------------------------
-- Model

share [mkPersist sqlSettings] [persistLowerCase|
Paste sql=paste
  title Text
  content Text
  author Text
  created UTCTime default=now()
  language LanguageId Maybe sqltype=integer
  deriving Show
Language sql=language
  name Text
  title Text
  ordinal Int
  visible Bool
  deriving Show
|]

--------------------------------------------------------------------------------
-- Routes

mkYesod "App" [parseRoutes|
  / NewPasteR GET POST
  /#PasteId GET
|]

--------------------------------------------------------------------------------
-- New paste page

data NewPaste = NewPaste
  { newPasteTitle :: Text
  , newPasteAuthor :: Text
  , newPasteLanguage :: LanguageId
  , newPasteContent :: Text
  } deriving (Show)

getNewPasteR :: Handler Html
getNewPasteR = do
  langs <- getLanguages
  (formWidget, enctype) <- generateFormPost (renderDivs (pasteForm langs))
  showForm formWidget enctype

postNewPasteR :: Handler Html
postNewPasteR = do
  langs <- getLanguages
  ((res, formWidget), enctype) <- runFormPost (renderDivs (pasteForm langs))
  case res of
    FormSuccess newPaste -> do
      now <- liftIO getCurrentTime
      pid <-
        runDB
          (do pid <- generatePasteId
              insertKey
                pid
                Paste
                { pasteTitle = newPasteTitle newPaste
                , pasteContent = newPasteContent newPaste
                , pasteAuthor = newPasteAuthor newPaste
                , pasteCreated = now
                , pasteLanguage = Just (newPasteLanguage newPaste)
                }
              pure pid)
      redirect (GET pid)
    _ -> showForm formWidget enctype

getLanguages :: Handler [Entity Language]
getLanguages = runDB (selectList [LanguageVisible ==. True] [])

showForm :: WidgetFor App () -> Enctype -> Handler Html
showForm formWidget enctype =
  defaultLayout $ do
    setTitle "New paste"
    [whamlet|
<style>
  form > div { margin: 1em; }
  label { padding-right: 1em; }
  input[type=text] { width: 30em; }
  textarea { display: block; margin-top: 1em; clear: both; width: 100%; height: 40em; font-family: monospace }
<body>
  <form method=post enctype=#{enctype}>
    ^{formWidget}
    <div>
      <input type=submit value="Paste">
|]

generatePasteId :: ReaderT SqlBackend (HandlerFor App) (Key Paste)
generatePasteId = do
  rows <-
    (runConduit
       (rawQuery "SELECT (RANDOM()*9223372036854775807) :: BIGINT" [] .|
        CL.consume))
  case rows of
    [[PersistInt64 pid]] -> do
      mr <- selectFirst [PasteId ==. toSqlKey pid] []
      case mr of
        Nothing -> pure (toSqlKey pid)
        Just {} -> generatePasteId
    _ -> error "Invalid generatePasteId result."

pasteForm :: [Entity Language] -> AForm Handler NewPaste
pasteForm langs =
  NewPaste <$> areq textField "Title" Nothing <*>
  areq textField "Author" Nothing <*>
  areq
    (selectFieldList
       (map (\(Entity key lang) -> (languageTitle lang, key)) langs))
    "Language"
    Nothing <*>
  fmap unTextarea (areq textareaField "Content" Nothing)

--------------------------------------------------------------------------------
-- Paste viewer page

handleGET :: PasteId -> Handler Html
handleGET pid = do
  mpaste <- runDB (get pid)
  case mpaste of
    Nothing -> notFound
    Just paste -> do
      language <- do
        case pasteLanguage paste of
          Nothing -> pure "none"
          Just lang -> do
            mlang <- runDB (get lang)
            case mlang of
              Nothing -> notFound
              Just language -> pure (languageName language)
      defaultLayout $ do
        setTitle (toHtml (pasteTitle paste))
        [whamlet|
<h1>
  #{pasteTitle paste}
<p>
  <em>
    #{pasteAuthor paste}
  #{show (pasteCreated paste)}
<pre>
  <code class=#{toHtml ("language-" <> language)}>
    #{pasteContent paste}
<link rel="stylesheet" href="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css">
<style>
 code.hljs { background: white; }
<script src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js">
<script src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/haskell.min.js">
<script src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/clojure.min.js">
<script src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/erlang.min.js">
<script src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/lisp.min.js">
<script src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/elm.min.js">
<script>
  hljs.tabReplace = '    ';
  hljs.initHighlightingOnLoad();
|]

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main =
  runNoLoggingT
    (withPostgresqlPool
       "dbname=lpaste user=lpaste password=lpaste"
       1
       (\pool -> liftIO (warpEnv (App pool))))

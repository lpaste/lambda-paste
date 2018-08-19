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
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Builder as L
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Maybe
import           Data.Monoid
import           Data.Pool
import           Data.String
import           Data.Text (Text)
import           Data.Time (getCurrentTime, UTCTime)
import           Database.Persist.Postgresql
import           Skylighting
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Yesod

--------------------------------------------------------------------------------
-- App

data App = App (Pool SqlBackend)

instance Yesod App where
  maximumContentLength _ _ = Just (1024 * 20)
  makeSessionBackend _ = return Nothing

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
RawPaste sql=paste
  content ByteString
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
  /#PasteId PasteR GET
  /raw/#RawPasteId RawR GET
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
      redirect (PasteR pid)
    _ -> showForm formWidget enctype

getLanguages :: Handler [Entity Language]
getLanguages = runDB (selectList [LanguageVisible ==. True] [Asc LanguageOrdinal])

showForm :: WidgetFor App () -> Enctype -> Handler Html
showForm formWidget enctype =
  defaultLayout $ do
    setTitle "New paste"
    [whamlet|
<body>
  <style>
    form > div { margin: 1em; }
    label { padding-right: 1em; }
    input[type=text] { width: 30em; }
    textarea { display: block; margin-top: 1em; clear: both; width: 100%; height: 40em; font-family: monospace }
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
  NewPaste <$> fmap (fromMaybe "No title") (aopt textField "Title" Nothing) <*>
  fmap (fromMaybe "Anonymous Coward") (aopt textField "Author" Nothing) <*>
  areq
    (selectFieldList
       (map (\(Entity key lang) -> (languageTitle lang, key)) langs))
    "Language"
    Nothing <*>
  fmap unTextarea (areq textareaField "Content" Nothing)

--------------------------------------------------------------------------------
-- Raw content

getRawR :: RawPasteId -> Handler RepPlain
getRawR pid = do
  mpaste <- runDB (get pid)
  case mpaste of
    Nothing -> notFound
    Just paste ->
      pure
        (RepPlain
           (ContentBuilder
              (L.byteString (rawPasteContent paste))
              (Just (S.length (rawPasteContent paste)))))

--------------------------------------------------------------------------------
-- Paste viewer page

getPasteR :: PasteId -> Handler Html
getPasteR pid = do
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
<style>
  table { font-family: monospace }
  table td { white-space: pre }
  td > a { color: #aaa; text-decoration: none; display: block; padding-right: 1em; user-select: none; -webkit-user-select: none; -moz-user-select: none; -ms-user-select: none; }
  .OtherTok, .CommentTok { color: #8e908c }
  .KeywordTok { color: #8959a8 }
  .DataTypeTok { color: #4271ae }
  .StringTok { color: #718c00}
<h1>
  #{pasteTitle paste}
<p>
  <em>
    #{pasteAuthor paste}
  #{show (pasteCreated paste)}
#{highlightAs language (pasteContent paste)}
|]

highlightAs :: Text -> Text -> H.Html
highlightAs lang src =
  case syntaxByName defaultSyntaxMap lang of
    Nothing -> H.pre (toHtml src)
    Just syntax ->
      case tokenize
             TokenizerConfig {syntaxMap = defaultSyntaxMap, traceOutput = False}
             syntax
             src of
        Left {} -> H.pre (toHtml src)
        Right toks -> tokensToHtml toks

tokensToHtml :: [[(TokenType, Text)]] -> H.Html
tokensToHtml =
  H.table .
  foldMap
    (\(i, line) ->
       H.tr H.! A.id ("line" <> fromString (show i)) $ do
         H.td
           (H.a H.! A.href ("#line" <> fromString (show i)) $ toHtml (show i))
         H.td
           (foldMap
              (\(ty, str) ->
                 H.span H.! A.class_ (fromString (show ty)) $ toHtml str)
              line)) .
  zip [1 :: Int ..]

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main =
  runNoLoggingT
    (withPostgresqlPool
       "dbname=lpaste user=lpaste password=lpaste"
       1
       (\pool -> liftIO (warpEnv (App pool))))

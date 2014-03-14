module Handlers where

import Import

import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Network.HTTP.Types

import Widgets


errorPage :: Status -> AppMessage -> Handler ()
errorPage status errorMessage = do
    html <- defaultLayout $(widgetFile "error")
    sendResponseStatus status html


getRootR :: Handler Html
getRootR = do
    host <- getHost
    url <- getUrl
    defaultLayout $ do
        setTitle $ toHtml host
        $(widgetFile "root")


ballotHead = [hamlet|
    <link rel="stylesheet" type="text/css" href="/static/css/ui-lightness/jquery-ui-1.8.17.custom.css" media="screen, projection" />
    <script type="text/javascript" src="/static/js/jquery-1.7.1.min.js">
    <script type="text/javascript" src="/static/js/jquery-ui-1.8.17.custom.min.js">
|]

getBallotFormR :: Handler Html
getBallotFormR = do
    host <- getHost
    defaultLayout $ do
        setTitleI MsgBallotFormTitle
        toWidgetHead ballotHead
        $(widgetFile "ballotForm")

postBallotFormR :: Handler Html
postBallotFormR = do
    -- Check upload rate limit
    uploadFrom <- getClientIP
    now <- liftIO getCurrentTime
    let yesterday = posixSecondsToUTCTime $ utcTimeToPOSIXSeconds now - 86400
    prevUploads <- runDB $ selectList [BallotUploadFrom ==. uploadFrom, BallotUploadTime >. yesterday] []
    if length prevUploads > 10
       then errorPage serviceUnavailable503 MsgPostRateExceeded
       else return ()
    -- Get form content
    ballot@(Ballot text images width height howManyToPrint _ _ _ _) <-
        runInputPost $ Ballot
                    <$> ireq textField "text"
                    <*> ireq textField "images"
                    <*> ireq intField "width"
                    <*> ireq intField "height"
                    <*> ireq intField "howManyToPrint"
                    <*> ireq intField "imgWidth"
                    <*> ireq intField "imgHeight"
                    <*> pure uploadFrom
                    <*> pure now
    -- Store in DB
    (ballotId, own) <- runDB $ do
        maybeId <- insertUnique ballot
        case maybeId of
             Just i -> return (i, True)
             Nothing -> do
                 maybeEntity <- getBy $ UniqueBallot text images width height howManyToPrint
                 case maybeEntity of
                      Just e -> return (entityKey e, False)
                      Nothing -> error "Try again"
    -- Update session
    if own
       then do
            maybePrevUploads <- lookupSession "uploads"
            let textToAdd = "," <> keyToText ballotId
            setSession "uploads" $ maybe (textToAdd) (<> textToAdd) maybePrevUploads
       else return ()
    -- Redirect
    redirect $ BallotByIdR ballotId


getUploadsFromSession = filter (not . T.null) <$> T.splitOn "," <$> maybe "" id <$> lookupSession "uploads"

keyToText key = pack $ show i
    where (PersistInt64 i) = unKey key

getBallotByIdR :: BallotId -> Handler Html
getBallotByIdR ballotId = do
    ballot <- runDB $ get404 ballotId
    host <- getHost
    url <- getUrl
    uploads <- getUploadsFromSession
    let ballotIdText = keyToText ballotId
        ownBallot = ballotIdText `elem` uploads
    defaultLayout $ do
        setTitleI $ MsgBallotByIdTitle ballotIdText
        toWidgetHead ballotHead
        toWidgetHead [hamlet|
            <style>
                #renderImages > img {
                    max-width: #{ballotImgWidth ballot}px;
                    max-height: #{ballotImgHeight ballot}px;
                }
        |]
        $(widgetFile "ballotById")

postBallotByIdR :: BallotId -> Handler ()
postBallotByIdR ballotId = do
    uploads <- getUploadsFromSession
    let ballotIdText = keyToText ballotId
    if ballotIdText `notElem` uploads
       then errorPage unauthorized401 MsgNotAuthorizedToDeleteBallot
       else do
           runDB $ delete ballotId
           setMessage $ toHtml $ "Bulletin n°" <> ballotIdText <> " supprimé."
           setSession "uploads" $ T.intercalate "," $ filter (/=ballotIdText) uploads
           redirect BallotFormR

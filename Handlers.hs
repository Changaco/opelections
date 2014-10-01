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
    url <- getUrl
    defaultLayout $ do
        setTitle "OpElections"
        $(widgetFile "root")


getBallotFormR :: Handler Html
getBallotFormR = do
    host <- getHost
    defaultLayout $ do
        setTitleI MsgBallotFormTitle
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
    ballot <-
        runInputPost $ Ballot
                    <$> idef textField "text" ""
                    <*> idef textField "secondaryText" ""
                    <*> idef textField "images" ""
                    <*> ireq intField "width"
                    <*> ireq intField "height"
                    <*> ireq intField "howManyToPrint"
                    <*> ireq doubleField "imgWidth"
                    <*> ireq doubleField "imgHeight"
                    <*> pure uploadFrom
                    <*> pure now
    -- Store in DB
    (ballotId, own) <- runDB $ do
        r <- insertBy ballot
        case r of
             Left e -> return (entityKey e, False)
             Right k -> return (k, True)
    -- Update session
    if own
       then do
            maybePrevUploads <- lookupSession "uploads"
            let textToAdd = "," <> tshow ballotId
            setSession "uploads" $ maybe (textToAdd) (<> textToAdd) maybePrevUploads
       else return ()
    -- Redirect
    redirect $ BallotByIdR ballotId


getUploadsFromSession = filter (not . T.null) <$> T.splitOn "," <$> maybe "" id <$> lookupSession "uploads"

getBallotByIdR :: BallotId -> Handler Html
getBallotByIdR ballotId = do
    ballot <- runDB $ get404 ballotId
    host <- getHost
    url <- getUrl
    uploads <- getUploadsFromSession
    let ballotIdText = tshow ballotId
        ownBallot = ballotIdText `elem` uploads
    defaultLayout $ do
        setTitleI $ MsgBallotByIdTitle ballotIdText
        $(widgetFile "ballotById")

postBallotByIdR :: BallotId -> Handler ()
postBallotByIdR ballotId = do
    uploads <- getUploadsFromSession
    let ballotIdText = tshow ballotId
    if ballotIdText `notElem` uploads
       then errorPage unauthorized401 MsgNotAuthorizedToDeleteBallot
       else do
           runDB $ delete ballotId
           setMessage $ toHtml $ "Bulletin n°" <> ballotIdText <> " supprimé."
           setSession "uploads" $ T.intercalate "," $ filter (/=ballotIdText) uploads
           redirect BallotFormR


getBallotListR :: Handler Html
getBallotListR = do
    host <- getHost
    let limit = 10
    offset <- do
        maybeOffset <- maybe (Just 0) readMay <$> lookupGetParam "offset"
        maybe (invalidArgs ["offset"]) return maybeOffset
    ballots <- runDB $
        selectList [] [Desc BallotUploadTime, LimitTo limit, OffsetBy offset]
    totalCount <- runDB $ count [BallotWidth >. 0]
    when (null ballots && offset /= 0) $ invalidArgs ["offset"]
    let prevOffset = if offset /= 0
                     then Just (offset - limit)
                     else Nothing
    let nextOffset = if offset + limit < totalCount
                     then Just (offset + limit)
                     else Nothing
    defaultLayout $ do
        setTitleI MsgBallotListTitle
        $(widgetFile "ballotList")

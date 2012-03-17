module Handler.Opelections where


import Database.Persist.Store
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Import
import Network.HTTP.Types
import Network.Wai


getHost = decodeUtf8 <$> serverName <$> waiRequest

getUrl = do
    host <- getHost
    renderer <- getUrlRender
    maybeRoute <- getCurrentRoute
    case maybeRoute of
         Just r -> return $ "http://" <> host <> renderer r
         Nothing -> return ""


shareWidget :: (RenderMessage Opelections a) => (Text -> a) -> Text -> Widget
shareWidget msg url = [whamlet|
    <a href="http://www.facebook.com/sharer.php?u=#{url}&t=_{msg url}"><img src="/static/img/facebook.png" />
    <a href="https://twitter.com/share?text=_{msg url}"><img src="/static/img/twitter.png" />
    <a href="http://identi.ca/index.php?action=newnotice&status_textarea=_{msg url}"><img src="/static/img/identi.png" />
|]


faqWidget = $(whamletFile "templates/faq.hamlet")

wikipediaFR :: Text -> Widget
wikipediaFR t = [whamlet|<a href="http://fr.wikipedia.org/wiki/#{t}">#{t}|]


errorPage :: Status -> OpelectionsMessage -> Handler ()
errorPage status errorMessage = do
    html <- defaultLayout $(widgetFile "error")
    sendResponseStatus status html


getRootR :: Handler RepHtml
getRootR = do
    host <- getHost
    url <- getUrl
    defaultLayout $ do
        setTitle $ toHtml host
        if host == "vote-utile.fr"
           then $(widgetFile "vote-utile")
           else $(widgetFile "root")


ballotHead = [hamlet|
    <link rel="stylesheet" type="text/css" href="/static/css/ui-lightness/jquery-ui-1.8.17.custom.css" media="screen, projection" />
    <script type="text/javascript" src="/static/js/jquery-1.7.1.min.js">
    <script type="text/javascript" src="/static/js/jquery-ui-1.8.17.custom.min.js">
|]

getBallotFormR :: Handler RepHtml
getBallotFormR = do
    host <- decodeUtf8 <$> serverName <$> waiRequest
    defaultLayout $ do
        setTitleI MsgBallotFormTitle
        addHamletHead ballotHead
        $(widgetFile "ballotForm")

postBallotFormR :: Handler RepHtml
postBallotFormR = do
    -- Check upload rate limit
    uploadFrom <- fromJust <$> lookup "X-Real-IP" <$> requestHeaders <$> waiRequest
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

keyToText key = showT i
    where (PersistInt64 i) = unKey key

getBallotByIdR :: BallotId -> Handler RepHtml
getBallotByIdR ballotId = do
    ballot <- runDB $ get404 ballotId
    host <- getHost
    url <- getUrl
    uploads <- getUploadsFromSession
    let msg = if host == "vote-utile.fr" then MsgShareVoteUtile else MsgShareText
        ballotIdText = keyToText ballotId
        ownBallot = ballotIdText `elem` uploads
    defaultLayout $ do
        setTitleI $ MsgBallotByIdTitle ballotIdText
        addHamletHead ballotHead
        addHamletHead [hamlet|
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

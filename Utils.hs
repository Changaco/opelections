module Utils where

import ClassyPrelude

import Data.Maybe
import Network.Wai
import Yesod


-- * i18n

plural :: (Num i, Eq i) => i -> Text -> Text -> Text
plural 1 a _ = a
plural _ _ b = b


-- * Synonyms for use in templates

one = 1 :: Int
eq = (==)


-- * WAI

getClientIP = fromJust <$> lookup "X-Real-IP" <$> requestHeaders <$> waiRequest

getHost = decodeUtf8 <$> fromJust <$> requestHeaderHost <$> waiRequest

getUrl = do
    renderer <- getUrlRender
    maybeRoute <- getCurrentRoute
    return $ "http:" <> renderer (fromJust maybeRoute)

module Widgets where

import Import


icon :: Text -> Text -> Widget
icon code alt = [whamlet|
<i class="fa fa-lg fa-#{code}"></i>
<span class="sr-only"> #{alt}
|]


shareWidget :: (RenderMessage App a) => (Text -> a) -> Text -> Widget
shareWidget msg url = [whamlet|
    <a class="shareLink" href="http://www.facebook.com/sharer.php?u=#{url}&t=_{msg url}">
        ^{icon "facebook-square" "Facebook"}
    <a class="shareLink" href="https://twitter.com/share?text=_{msg url}">
        ^{icon "twitter" "Twitter"}
    <a class="shareLink" href="https://plus.google.com/share?url=#{url}">
        ^{icon "google-plus" "Google+"}
|]


faqWidget = $(whamletFile "templates/faq.hamlet")


wikipediaFR :: Text -> Widget
wikipediaFR t = [whamlet|<a href="http://fr.wikipedia.org/wiki/#{t}">#{t}|]

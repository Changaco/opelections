module Widgets where

import Import

import qualified Data.Text as T


ballotWidget :: Text -> Ballot -> Widget
ballotWidget host Ballot{..} = [whamlet|
<div class="ballot" style="width: #{ballotWidth}cm; height: #{ballotHeight}cm">
    <div class="ballotWrapper">
        <div class="ballotTextWrapper">
            <div class="ballotText">#{ballotText}
            <div class="ballotSecondaryText">#{ballotSecondaryText}
        <div class="ballotImages">
            <span class="valign">
            $forall src <- T.lines ballotImages
                <img src="#{src}" alt="_{MsgImageLoadFailed}"
                     style="max-height: #{ballotImgHeight}px; max-width: #{ballotImgWidth}px;" />
    <span class="valign">
    <div class="genmsg">_{MsgGenerator host}
|]


faqWidget = $(whamletFile "templates/faq.hamlet")


icon :: Text -> Text -> Widget
icon code alt = [whamlet|
<i class="fa fa-lg fa-#{code}"></i>
<span class="sr-only"> #{alt}
|]


navWidget :: Text -> Widget
navWidget routeName =
    let isRootR = routeName == "RootR"
        isBallotFormR = routeName == "BallotFormR"
        isBallotListR = routeName == "BallotListR"
    in  [whamlet|
<nav id="nav">
    <ul class="nav nav-pills">
        <li :isRootR:class="active">
            <a href="@{RootR}">_{MsgRootR}
        <li :isBallotFormR:class="active">
            <a href="@{BallotFormR}">_{MsgBallotFormR}
        <li :isBallotListR:class="active">
            <a href="@{BallotListR}">_{MsgBallotListR}
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


wikipediaFR :: Text -> Widget
wikipediaFR t = [whamlet|<a href="http://fr.wikipedia.org/wiki/#{t}">#{t}|]

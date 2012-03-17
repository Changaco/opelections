module Import
    ( module Prelude
    , module Yesod
    , module Foundation
    , module Data.Monoid
    , module Control.Applicative
--     , module Settings.StaticFiles
    , module Data.Text.Encoding
    , module Data.Time
    , Text
#if __GLASGOW_HASKELL__ < 740
    , (<>)
#endif
    , showT
    ) where

import Prelude hiding (writeFile, readFile, head, tail, init, last)
import Yesod   hiding (Route(..))
import Foundation
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text, pack)
-- import Settings.StaticFiles

import Data.Text.Encoding
import Data.Time hiding (parseTime)

#if __GLASGOW_HASKELL__ < 740
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

showT a = pack $ show a

module Model where

import Prelude
import Yesod
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time
import Data.Typeable (Typeable)
import Database.Persist.Quasi

share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

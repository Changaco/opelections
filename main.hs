import Prelude
import Yesod.Default.Config
import Yesod.Logger         (defaultDevelopmentLogger)
import Settings             (parseExtra)
import Application          (getApplication)
import Network.Socket
import Network.Wai.Handler.Warp
    (runSettings, runSettingsSocket, defaultSettings, settingsPort)

main :: IO ()
main = do
    config <- fromArgs parseExtra
    logger <- defaultDevelopmentLogger
    app <- getApplication config logger
    let settings = defaultSettings { settingsPort = appPort config }
    if appPort config == 0
       then do
            sock <- socket AF_UNIX Stream defaultProtocol
            bindSocket sock $ SockAddrUnix "/tmp/opelections.sock"
            listen sock 1
            runSettingsSocket settings sock app
       else
            runSettings settings app

import Prelude
import Yesod.Default.Config
import Settings             (parseExtra)
import Application          (getApplication)
import Network.Socket
import Network.Wai.Handler.Warp
    (runSettings, runSettingsSocket, defaultSettings, settingsPort)

main :: IO ()
main = do
    config <- fromArgs parseExtra
    app <- getApplication config
    let settings = defaultSettings { settingsPort = appPort config }
    if appPort config == 0
       then do
            sock <- socket AF_UNIX Stream defaultProtocol
            bindSocket sock $ SockAddrUnix "socket"
            listen sock 1
            runSettingsSocket settings sock app
       else
            runSettings settings app

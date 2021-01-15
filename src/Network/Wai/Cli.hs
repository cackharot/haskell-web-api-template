{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Network.Wai.Cli where

import qualified Network.Wai.Handler.CGI              as CGI
import           Network.Wai.Handler.Warp             hiding (run)
#ifdef WaiCliTLS
import           Network.Wai.Handler.WarpTLS
#endif
#ifdef WaiCliFastCGI
import qualified Network.Wai.Handler.FastCGI          as FCGI
#endif
import           Control.Exception                    (bracket)
import           Data.IP                              (fromHostAddress,
                                                       fromHostAddress6)
import           Data.List                            (intercalate)
import           Data.Streaming.Network               (bindPortTCP)
import           Data.String                          (fromString)
import           GHC.Conc                             (getNumCapabilities)
import qualified Network.Socket                       as S
import           Network.Wai                          (Application)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Options
import           System.Console.ANSI

data GracefulMode = ServeNormally | Serve503

data WaiOptions = WaiOptions
  { wHttpPort    ∷ Int
  , wHttpHost    ∷ String
#ifdef WaiCliUnix
  , wUnixSock    ∷ String
#endif
  , wProtocol    ∷ String
#ifdef WaiCliTLS
  , wTlsKeyFile  ∷ String
  , wTlsCertFile ∷ String
#endif
  , wDevlogging  ∷ Maybe Bool }

instance Options WaiOptions where
  defineOptions = pure WaiOptions
    <*> simpleOption "port"              3000                "The port the app should listen for connections on (for http)"
    <*> simpleOption "host"              "*4"                "Host preference (for http)"
    <*> simpleOption "protocol"          "http"              ("The protocol for the server. One of: " ++ availableProtocols)
#ifdef WaiCliTLS
    <*> simpleOption "tlskey"            ""                  "Path to the TLS private key file for +tls protocols"
    <*> simpleOption "tlscert"           ""                  "Path to the TLS certificate bundle file for +tls protocols"
#endif
    <*> simpleOption "devlogging"        Nothing             "Whether development logging should be enabled"
    where
      availableProtocols = intercalate ", " [
          "http", "cgi"
#ifdef WaiCliTLS
        , "http+tls"
#endif
#ifdef WaiCliFastCGI
        , "fastcgi"
#endif
        ]

-- | Adjusts 'WaiOptions' with an address assigned to a newly created
-- server socket, uses those to set a "before main loop" function in
-- Warp 'Settings', which are then used to run an application.
runWarp :: (WaiOptions -> IO ())
        -- ^ A "before main loop" function
        -> WaiOptions
        -- ^ Original options
        -> (Settings -> S.Socket -> Application -> IO ())
        -- ^ A function such as 'runSettingsSocket'
        -> Settings -> Application -> IO ()
runWarp putListening opts runSocket set app = S.withSocketsDo $
  bracket (bindPortTCP (getPort set) (getHost set)) S.close $ \s -> do
  sa <- S.getSocketName s
  S.withFdSocket s $ S.setCloseOnExecIfNeeded
  -- S.setCloseOnExecIfNeeded $ S.fdSocket s
  runSocket (setBeforeMainLoop (putListening $ updateOptions sa opts) set) s app
  where
    updateOptions :: S.SockAddr -> WaiOptions -> WaiOptions
    updateOptions (S.SockAddrInet pn ha) opt =
      opt { wHttpPort = fromIntegral pn, wHttpHost = show (fromHostAddress ha) }
    updateOptions (S.SockAddrInet6 pn _flow ha _scope) opt =
      opt { wHttpPort = fromIntegral pn, wHttpHost = show (fromHostAddress6 ha) }
    updateOptions _ opt = opt

waiMain ∷ (WaiOptions → IO ()) → (WaiOptions → IO ()) → Application → IO ()
waiMain putListening putWelcome app = runCommand $ \opts _ → do
#ifdef WaiCliTLS
  let tlss = tlsSettings (wTlsCertFile opts) (wTlsKeyFile opts)
#endif
  let warps = setBeforeMainLoop (putListening opts) $ setPort (wHttpPort opts) $
              setHost (fromString $ wHttpHost opts) defaultSettings
  let app' = if wDevlogging opts == Just True then logStdoutDev app else app
  case wProtocol opts of
     "cgi" → CGI.run app'
#ifdef WaiCliFastCGI
     "fastcgi" → FCGI.run app'
#endif
     _ → do
       let run = case wProtocol opts of
             "http"     → runWarp putListening opts runSettingsSocket
#ifdef WaiCliTLS
             "http+tls" → runWarp putListening opts (runTLSSocket tlss)
#endif
             x          → \_ _ → putStrLn $ "Unsupported protocol: " ++ x
       putWelcome opts
       run warps app'

defPutListening ∷ WaiOptions → IO ()
defPutListening opts = getNumCapabilities >>= putMain
  where putMain cpus = reset "Running on " >> blue (wProtocol opts) >> putProto >> reset " with " >> green (show cpus ++ " CPUs") >> setReset >> putStrLn ""
        putProto = case wProtocol opts of
                     "http" → reset " host " >> boldMagenta (wHttpHost opts)
                              >> reset ", port " >> boldMagenta (show $ wHttpPort opts)
#ifdef WaiCliTLS
                     "http+tls" → reset " (TLS) port "   >> boldMagenta (show $ wHttpPort opts)
#endif
                     _      → setReset
        setReset = setSGR [ Reset ]
        boldMagenta x = setReset >> setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Magenta ] >> putStr x
        green x = setReset >> setSGR [ SetColor Foreground Dull Green ] >> putStr x
        blue  x = setReset >> setSGR [ SetColor Foreground Dull Blue ] >> putStr x
        reset x = setReset >> putStr x

defWaiMain ∷ Application → IO ()
defWaiMain = waiMain defPutListening (\_ → return ())

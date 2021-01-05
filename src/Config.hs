module Config
where

import qualified System.Envy as Envy
import           System.IO   (hPutStrLn)
import           System.IO   (stderr)

-- Read from ENVIRONMENT Variables
withAppSettingsFromEnv :: Envy.FromEnv t => (t -> IO ()) -> IO ()
withAppSettingsFromEnv f = Envy.decodeEnv >>= callF
  where
    callF x =
      case x of
        Left e  -> hPutStrLn stderr ("Error reading env: " ++ e)
        Right c -> f c

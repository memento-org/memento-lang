{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}


module Language.Memento.LSP.Server
  ( runMementoServer
  ) where

import qualified Colog.Core                    as C
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Text                     (Text)
import           Language.LSP.Logging
import           Language.LSP.Protocol.Types
import           Language.LSP.Server
import           Language.Memento.LSP.Handlers (handlers)
import           System.IO                     (hPutStrLn, stderr)

----------------------------------------------------------------------
-- 2. VS Code 側へ流すロガー（初期化後）
----------------------------------------------------------------------
clientLogger :: C.LogAction (LspM ()) (C.WithSeverity Text)
clientLogger = defaultClientLogger      -- ここを書き換えればカスタム可

logMessage :: C.Severity -> Text -> LspM () ()
logMessage severity msg = C.unLogAction clientLogger
  $ C.WithSeverity msg severity


-- | Run the Memento Language Server
runMementoServer :: IO ()
runMementoServer = do
  hPutStrLn stderr "[Info] Starting Memento LSP server"
  result <- Language.LSP.Server.runServer serverDefinition
  hPutStrLn stderr $ "[Info] Server stopped with result: " ++ show result
  pure ()

-- | Server definition with configuration
serverDefinition :: ServerDefinition ()
serverDefinition = ServerDefinition
  { parseConfig = const $ const $ Right ()
  , onConfigChange = const $ pure ()
  , defaultConfig = ()
  , configSection = "memento"
  , doInitialize = \env _req -> do
      hPutStrLn stderr "[Debug] Initializing LSP server"
      pure $ Right env
  , staticHandlers = \_caps -> handlers logMessage
  , interpretHandler = \env -> Iso (runLspT env) liftIO
  , options = mementoOptions
      { optTextDocumentSync = Just $ TextDocumentSyncOptions
          { _openClose = Just True
          , _change = Just TextDocumentSyncKind_Full
          , _willSave = Nothing
          , _willSaveWaitUntil = Nothing
          , _save = Just $ InR $ SaveOptions { _includeText = Just True }
          }
      }
  }

-- | Default options for the server
mementoOptions :: Options
mementoOptions = Language.LSP.Server.defaultOptions
  { optServerInfo = Just $ ServerInfo
      { _name = "memento-lsp" :: Text
      , _version = Just "0.1.0.0" :: Maybe Text
      }
  }

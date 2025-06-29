{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DisambiguateRecordFields  #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

module Language.Memento.LSP.Server
  ( runMementoServer
  ) where

import           Colog.Core                                (LogAction (..),
                                                            WithSeverity (..),
                                                            (<&))
import           Control.Monad.IO.Class                    (MonadIO (..))
import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server
import           Language.Memento.LSP.Handlers             (handlers)
import           Prettyprinter                             (defaultLayoutOptions,
                                                            layoutPretty,
                                                            pretty)
import qualified Prettyprinter.Render.Text                 as PP
import           System.IO                                 (stderr)

-- | Run the Memento Language Server
runMementoServer :: IO ()
runMementoServer = do
  putStrLn "[Info] Starting server"
  result <- Language.LSP.Server.runServer serverDefinition
  putStrLn $ "[Info] Server stopped with result: " ++ show result
  pure ()

-- | Server definition with configuration
serverDefinition :: ServerDefinition ()
serverDefinition = ServerDefinition
  { parseConfig = const $ const $ Right ()
  , onConfigChange = const $ pure ()
  , defaultConfig = ()
  , configSection = "memento"
  , doInitialize = \env _req -> do
      putStrLn "[Debug] Initializing LSP server"
      pure $ Right env
  , staticHandlers = \_caps -> handlers
  , interpretHandler = \env -> Iso (runLspT env) liftIO
  , options = defaultOptions
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
      { _name = ("memento-lsp" :: Text)
      , _version = (Just "0.1.0.0" :: Maybe Text)
      }
  }
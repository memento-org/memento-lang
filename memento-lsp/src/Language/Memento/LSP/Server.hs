{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Language.Memento.LSP.Server
  ( runServer
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
runServer :: IO ()
runServer = do
  exitCode <- runServerWith serverDefinition
  case exitCode of
    0 -> putStrLn "Server exited successfully"
    n -> putStrLn $ "Server exited with code: " ++ show n

-- | Server definition with configuration
serverDefinition :: ServerDefinition ()
serverDefinition = ServerDefinition
  { parseConfig = const $ const $ Right ()
  , onConfigChange = const $ pure ()
  , defaultConfig = ()
  , configSection = "memento"
  , doInitialize = \env _req -> pure $ Right env
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
defaultOptions :: Options
defaultOptions = defaultOptions
  { optServerInfo = Just $ ServerInfo
      { _name = "memento-lsp"
      , _version = Just "0.1.0.0"
      }
  }
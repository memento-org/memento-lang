{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}

module Language.Memento.LSP.Handlers
  ( handlers
  ) where

import qualified Colog.Core                       as C
import           Control.Monad.IO.Class           (liftIO)
import qualified Data.Map                         as Map
import qualified Data.SortedList                  as SortedList
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server              (Handlers, LspM,
                                                   notificationHandler,
                                                   publishDiagnostics,
                                                   requestHandler)
import           Language.Memento.LSP.Diagnostics (computeDiagnostics)
import           System.Exit                      (exitSuccess)

-- | All message handlers for the LSP server
handlers :: (C.Severity -> Text -> LspM () ()) -> Handlers (LspM ())
handlers logMessage = mconcat
  [ requestHandler SMethod_Shutdown $ \_req respond -> do
      respond (Right Null)
      pure ()
  , notificationHandler SMethod_Exit $ \_notif -> liftIO exitSuccess
  , notificationHandler SMethod_Initialized $ \_not -> do
      logMessage C.Debug "Memento LSP initialized successfully"

  , notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
      logMessage C.Debug "Document opened"
      let TNotificationMessage _ _ (DidOpenTextDocumentParams doc) = msg
          TextDocumentItem uri _ _ content = doc
      logMessage C.Debug ("Processing file: " <> T.pack (show uri))

      -- Compute real diagnostics using Memento compiler
      logMessage C.Debug "Running Memento compiler diagnostics..."
      diagnostics <- liftIO $ computeDiagnostics uri content
      logMessage C.Debug ("Found " <> T.pack (show (length diagnostics)) <> " diagnostics")

      -- Publish the diagnostics
      let diagnosticsMap = Map.fromList [(Nothing, SortedList.toSortedList diagnostics)]
      publishDiagnostics 100 (toNormalizedUri uri) Nothing diagnosticsMap
      logMessage C.Debug "Published real compiler diagnostics"

  , notificationHandler SMethod_TextDocumentDidSave $ \msg -> do
      logMessage C.Debug "Document saved"
      let TNotificationMessage _ _ (DidSaveTextDocumentParams doc _maybeContent) = msg
          TextDocumentIdentifier uri = doc
      -- Read fresh content from disk for real-time diagnostics
      let uriText = getUri uri
          filePath = if T.isPrefixOf "file://" uriText
                     then T.unpack $ T.drop 7 uriText  -- Remove "file://" prefix
                     else T.unpack uriText
      fileContent <- liftIO $ readFile filePath
      let content = T.pack fileContent
      logMessage C.Debug ("Processing saved file: " <> T.pack (show uri))

      -- Compute real diagnostics using Memento compiler
      logMessage C.Debug "Running Memento compiler diagnostics..."
      diagnostics <- liftIO $ computeDiagnostics uri content
      logMessage C.Debug ("Found " <> T.pack (show (length diagnostics)) <> " diagnostics")

      -- Publish the diagnostics
      let diagnosticsMap = Map.fromList [(Nothing, SortedList.toSortedList diagnostics)]
      publishDiagnostics 100 (toNormalizedUri uri) Nothing diagnosticsMap
      logMessage C.Debug "Published real compiler diagnostics"

  , notificationHandler SMethod_TextDocumentDidClose $ \msg -> do
      logMessage C.Debug "Document closed"
      let TNotificationMessage _ _ (DidCloseTextDocumentParams doc) = msg
          TextDocumentIdentifier uri = doc
      logMessage C.Debug ("Closed file: " <> T.pack (show uri))
      -- Clear diagnostics for the closed file
      let diagnosticsMap = Map.fromList [(Nothing, SortedList.toSortedList [])]
      publishDiagnostics 100 (toNormalizedUri uri) Nothing diagnosticsMap
  ]

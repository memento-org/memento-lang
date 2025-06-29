{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Language.Memento.LSP.Handlers
  ( handlers
  ) where

import           Control.Monad.IO.Class        (liftIO)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server
import           Language.Memento.LSP.Diagnostics (computeDiagnostics)

-- | All message handlers for the LSP server
handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SMethod_Initialized $ \_not -> do
      liftIO $ putStrLn "Memento LSP initialized"
  
  , notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
      let TNotificationMessage _ _ (DidOpenTextDocumentParams doc) = msg
          TextDocumentItem uri _ _ content = doc
      diagnostics <- liftIO $ computeDiagnostics uri content
      publishDiagnostics 100 (toNormalizedUri uri) Nothing diagnostics
  
  , notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
      let TNotificationMessage _ _ (DidChangeTextDocumentParams doc changes) = msg
          VersionedTextDocumentIdentifier uri _ = doc
      case changes of
        [TextDocumentContentChangeEvent _ _ content] -> do
          diagnostics <- liftIO $ computeDiagnostics uri content
          publishDiagnostics 100 (toNormalizedUri uri) Nothing diagnostics
        _ -> pure ()
  
  , notificationHandler SMethod_TextDocumentDidSave $ \msg -> do
      let TNotificationMessage _ _ (DidSaveTextDocumentParams doc _) = msg
          TextDocumentIdentifier uri = doc
      liftIO $ putStrLn $ "Document saved: " ++ show uri
  
  , notificationHandler SMethod_TextDocumentDidClose $ \msg -> do
      let TNotificationMessage _ _ (DidCloseTextDocumentParams doc) = msg
          TextDocumentIdentifier uri = doc
      -- Clear diagnostics for closed documents
      publishDiagnostics 100 (toNormalizedUri uri) Nothing []
  
  , requestHandler SMethod_TextDocumentHover $ \req responder -> do
      let TRequestMessage _ _ _ (HoverParams doc pos _workDone) = req
      -- TODO: Implement hover functionality
      responder $ Right $ InL Null
  
  , requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
      let TRequestMessage _ _ _ (DefinitionParams doc pos _workDone _partial) = req
      -- TODO: Implement go to definition
      responder $ Right $ InR $ InR Null
  
  , requestHandler SMethod_TextDocumentCompletion $ \req responder -> do
      let TRequestMessage _ _ _ (CompletionParams doc pos _workDone _partial _context) = req
      -- TODO: Implement completion
      responder $ Right $ InL $ CompletionList False Nothing []
  ]
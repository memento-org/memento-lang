{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DisambiguateRecordFields  #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

module Language.Memento.LSP.Handlers
  ( handlers
  ) where

import           Control.Monad.IO.Class        (liftIO)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server
import           Language.LSP.Diagnostics
import qualified Data.Map as Map
import qualified Data.SortedList as SortedList
import           Language.Memento.LSP.Diagnostics (computeDiagnostics)

-- | All message handlers for the LSP server
handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SMethod_Initialized $ \_not -> do
      liftIO $ putStrLn "[Debug] Memento LSP initialized successfully"
  
  , notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
      liftIO $ putStrLn "[Debug] Document opened"
      let TNotificationMessage _ _ (DidOpenTextDocumentParams doc) = msg
          TextDocumentItem uri _ _ content = doc
      liftIO $ putStrLn $ "[Debug] Processing file: " ++ show uri
      
      -- Compute real diagnostics using Memento compiler
      liftIO $ putStrLn "[Debug] Running Memento compiler diagnostics..."
      diagnostics <- liftIO $ computeDiagnostics uri content
      liftIO $ putStrLn $ "[Debug] Found " ++ show (length diagnostics) ++ " diagnostics"
      
      -- Publish the diagnostics
      let diagnosticsMap = Map.fromList [(Nothing, SortedList.toSortedList diagnostics)]
      publishDiagnostics 100 (toNormalizedUri uri) Nothing diagnosticsMap
      liftIO $ putStrLn "[Debug] Published real compiler diagnostics"
  ]
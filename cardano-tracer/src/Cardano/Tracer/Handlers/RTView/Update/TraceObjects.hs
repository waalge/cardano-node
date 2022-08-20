{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.TraceObjects
  ( updateUIBySavedTOs
  ) where

--import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM_, void, when)
import           Control.Monad.Extra (whenJustM, whenM)
import qualified Data.Text as T
import           Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.Charts
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.JS.Utils
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Utils
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

updateUIBySavedTOs
  :: TracerEnv
  -> LogsLiveViewCounters
  -> UI ()
updateUIBySavedTOs tracerEnv@TracerEnv{teSavedTO} llvCounters =
  whenM logsLiveViewIsOpened $ do
    window <- askWindow
    whenJustM (UI.getElementById window "node-logs-live-view-tbody") $ \el ->
      forConnectedUI_ tracerEnv $ \nodeId -> do
        nodeName        <- liftIO $ askNodeName tracerEnv nodeId
        nodeColor       <- liftIO $ getSavedColorForNode nodeName
        tosFromThisNode <- liftIO $ getTraceObjects teSavedTO nodeId
        forM_ tosFromThisNode $ \trObInfo -> do
          doAddItemRow nodeId nodeName nodeColor llvCounters el trObInfo
          -- Since we have added a new item row, we have to check if there are
          -- too many items already. If so - we have to remove old item row,
          -- to prevent too big number of them (if the user opened the window
          -- for a long time).
          liftIO (getLogsLiveViewCounter llvCounters nodeId) >>= \case
            Nothing -> return ()
            Just currentNumber ->
              when (currentNumber > maxNumberOfLogsLiveViewItems) $ do
                -- Ok, we have to delete outdated item row.
                let !outdatedItemNumber = currentNumber - maxNumberOfLogsLiveViewItems
                    outdatedItemId = nodeName <> "llv" <> showT outdatedItemNumber
                findAndDo window outdatedItemId delete'
 where
  logsLiveViewIsOpened = do
    window <- askWindow
    UI.getElementById window "logs-live-view-modal-window" >>= \case
      Nothing -> return False
      Just el -> (==) "opened" <$> get dataState el
  -- TODO: Probably it will be configured by the user.
  maxNumberOfLogsLiveViewItems = 20

doAddItemRow
  :: NodeId
  -> NodeName
  -> Maybe Color
  -> LogsLiveViewCounters
  -> Element
  -> (Namespace, TraceObjectInfo)
  -> UI ()
doAddItemRow nodeId@(NodeId anId) nodeName nodeColor
             llvCounters parentEl (ns, (msg, sev, ts)) = do
  liftIO $ incLogsLiveViewCounter llvCounters nodeId
  aRow <- mkItemRow
  void $ element parentEl #+ [aRow]
 where
  mkItemRow = do
    copyItemIcon <- image "has-tooltip-multiline has-tooltip-left rt-view-copy-icon" copySVG
                          # set dataTooltip "Click to copy this error"
    on UI.click copyItemIcon . const $ copyTextToClipboard $
      "[" <> preparedTS ts <> "] [" <> show sev <> "] [" <> T.unpack ns <> "] [" <> T.unpack msg <> "]"

    nodeNameLabel <-
      case nodeColor of
        Nothing -> UI.span # set text (T.unpack nodeName)
        Just (Color code) -> UI.span # set style [("color", code)]
                                     # set text (T.unpack nodeName)

    logItemRowId <-
      liftIO (getLogsLiveViewCounter llvCounters nodeId) >>= \case
        Nothing -> return $ T.unpack nodeName <> "llv0"
        Just currentNumber -> return $ T.unpack nodeName <> "llv" <> show currentNumber

    return $
      UI.tr ## logItemRowId #. (T.unpack anId <> "-node-logs-live-view-row") #+
        [ UI.td #+
            [ element nodeNameLabel
            ]
        , UI.td #+
            [ UI.span # set text (preparedTS ts)
            ]
        , UI.td #+
            [ UI.span #. "tag is-medium is-info" # set text (show sev)
            ]
        , UI.td #+
            [ UI.p #. "control" #+
                [ UI.input #. "input rt-view-error-msg-input"
                           # set UI.type_ "text"
                           # set (UI.attr "readonly") "readonly"
                           # set UI.value (T.unpack ns)
                ]
            ]
        , UI.td #+
            [ UI.p #. "control" #+
                [ UI.input #. "input rt-view-error-msg-input"
                           # set UI.type_ "text"
                           # set (UI.attr "readonly") "readonly"
                           # set UI.value (T.unpack msg)
                ]
            ]
        , UI.td #+
            [ element copyItemIcon
            ]
        ]

  preparedTS = formatTime defaultTimeLocale "%b %e, %Y %T"

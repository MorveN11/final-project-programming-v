{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Game (Action (Initialize), initialModel)
import Logic (updateModel)
import Miso
  ( App
      ( App,
        events,
        initialAction,
        logLevel,
        model,
        mountPoint,
        subs,
        update,
        view
      ),
    LogLevel (Off),
    defaultEvents,
    startApp,
  )
import Rendering (viewModel)

main :: IO ()
main = startApp App {..}
  where
    initialAction = Initialize
    model = initialModel
    update = updateModel
    view = viewModel
    events = defaultEvents
    subs = []
    mountPoint = Nothing
    logLevel = Off

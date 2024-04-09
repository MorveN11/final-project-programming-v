module Main where

import Game (initialModel)
import Miso
import Rendering (updateModel, viewModel)

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

module Logic (updateModel) where

import Game (Action (..), Model ())
import Miso (Effect, noEff)

updateModel :: Action -> Model -> Effect Action Model
updateModel Initialize m = noEff m
updateModel MoveUp m = noEff m
updateModel MoveDown m = noEff m
updateModel MoveLeft m = noEff m
updateModel MoveRight m = noEff m

module W40K.Data.Tau where

import Prelude hiding (Functor(..), Monad(..))

import Control.Lens
import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Data.Common


-- WARGEAR

novaShield :: Model -> Model
novaShield m = m
  & model_inv   .~ 3
  & model_name <>~ " (nova shield)"


-- MODELS

commander :: Model
commander = meq
  & model_class .~ Battlesuit
  & model_bs    .~ 2
  & model_str   .~ 5
  & model_tgh   .~ 5
  & model_wnd   .~ 6
  & model_att   .~ 4
  & model_ld    .~ 9
  & model_name  .~ "tau commander"

ethereal :: Model
ethereal = meq
  & model_bs   .~ 4
  & model_str  .~ 3
  & model_tgh  .~ 3
  & model_wnd  .~ 4
  & model_att  .~ 3
  & model_ld   .~ 9
  & model_save .~ 5
  & model_name .~ "ethereal"

fireWarrior :: Model
fireWarrior = meq
  & model_ws   .~ 5
  & model_bs   .~ 4
  & model_str  .~ 3
  & model_tgh  .~ 3
  & model_save .~ 4
  & model_ld   .~ 6
  & model_name .~ "fire warrior"

fireWarriorShas'ui :: Model
fireWarriorShas'ui = fireWarrior
  & model_wnd  .~ 2
  & model_ld   .~ 7
  & model_name .~ "fire warrior shas'ui"

pathfinder :: Model
pathfinder = fireWarrior
  & model_save .~ 5
  & model_name .~ "tau pathfinder"

krootCarnivore :: Model
krootCarnivore = meq
  & model_bs   .~ 4
  & model_str  .~ 3
  & model_tgh  .~ 3
  & model_ld   .~ 6
  & model_save .~ 6
  & model_name .~ "kroot carnivore"

stealthsuit :: Model
stealthsuit = meq
  & model_ws               .~ 5
  & model_bs               .~ 4
  & model_wnd              .~ 2
  & model_att              .~ 2
  & model_mods.mod_tobehit .~ -1
  & model_name             .~ "tau stealthsuit"

crisis :: Model
crisis = commander
  & model_ws    .~ 5
  & model_bs    .~ 4
  & model_wnd   .~ 3
  & model_att   .~ 2
  & model_ld    .~ 7
  & model_name  .~ "crisis battlesuit"

broadside :: Model
broadside = crisis
  & model_wnd  .~ 6
  & model_save .~ 2
  & model_name .~ "broadside"

ghostkeel :: Model
ghostkeel = stealthsuit
  & model_class .~ Battlesuit
  & model_str   .~ 6
  & model_tgh   .~ 6
  & model_wnd   .~ 10
  & model_ld    .~ 8
  & model_name  .~ "ghostkeel"

riptide :: Model
riptide = broadside
  & model_tgh  .~ 7
  & model_wnd  .~ 14
  & model_save .~ 2
  & model_inv  .~ 5
  & model_name .~ "riptide"

stormsurge :: Model
stormsurge = riptide
  & model_str  .~ 8
  & model_wnd  .~ 20
  & model_save .~ 3
  & model_name .~ "stormsurge"

devilfish :: Model
devilfish = rhino
  & model_bs   .~ 4
  & model_wnd  .~ 12
  & model_name .~ "devilfish"

hammerhead :: Model
hammerhead = devilfish
  & model_wnd  .~ 13
  & model_name .~ "hammerhead"

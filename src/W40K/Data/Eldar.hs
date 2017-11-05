module W40K.Data.Eldar where

import Prelude hiding (Functor(..), Monad(..))
import Control.Lens

import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Data.Common


-- MODELS

wraithguard :: Model
wraithguard = meq
  & model_str  .~ 5
  & model_tgh  .~ 5
  & model_wnd  .~ 3
  & model_att  .~ 1
  & model_ld   .~ 9
  & model_name .~ "wraithguard"

shieldWraithblade :: Model
shieldWraithblade = wraithguard
  & model_att  .~ 2
  & model_inv  .~ 4
  & model_name .~ "wraithblade (with shield)"

warWalker :: Model
warWalker = rhino
  & model_ws   .~ 3
  & model_str  .~ 5
  & model_tgh  .~ 6
  & model_wnd  .~ 6
  & model_att  .~ 2
  & model_ld   .~ 8
  & model_inv  .~ 5
  & model_name .~ "war walker"

waveSerpent :: Model
waveSerpent = rhino
  & model_str                  .~ 6
  & model_wnd                  .~ 13
  & model_ld                   .~ 8
  & model_rng_mods.mod_recvdmg .~ Add (-1)
  & model_fnp                  .~ 6
  & model_name                 .~ "wave serpent"

wraithlord :: Model
wraithlord = wraithguard
  & model_str  .~ 7
  & model_tgh  .~ 7
  & model_att  .~ 3
  & model_wnd  .~ 10
  & model_name .~ "wraithlord"

wraithknight :: Model
wraithknight = wraithguard
  & model_str  .~ 8
  & model_tgh  .~ 8
  & model_wnd  .~ 24
  & model_att  .~ 4
  & model_name .~ "wraithknight"

ravager :: Model
ravager = rhino
  & model_ws   .~ 4
  & model_tgh  .~ 6
  & model_ld   .~ 7
  & model_save .~ 4
  & model_inv  .~ 5
  & model_name .~ "ravager"


-- WEAPONS

wraithcannon :: RngWeapon
wraithcannon = lascannon
  & rw_str    .~ 10
  & rw_ap     .~ -4
  & rw_class  .~ Assault
  & rw_name   .~ "wraithcannon"

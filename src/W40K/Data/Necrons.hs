module W40K.Data.Necrons where

import Prelude hiding (Functor(..), Monad(..))

import Control.Lens
import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Data.Common

import Debug.Trace (trace)


-- MODELS

warrior :: Model
warrior = meq
  & model_ld   .~ 10
  & model_save .~ 4
  & model_name .~ "necron warrior"

immortal :: Model
immortal = warrior
  & model_save .~ 3
  & model_name .~ "necron immortal"

deathmark :: Model
deathmark = immortal
  & model_name .~ "necron deathmark"

lychguard :: Model
lychguard = immortal
  & model_str .~ 5
  & model_tgh .~ 5
  & model_wnd .~ 2
  & model_att .~ 2
  & model_name .~ "necron lychguard"

praetorian :: Model
praetorian = lychguard
  & model_name .~ "necron praetorian"

triarchStalker :: Model
triarchStalker = immortal
  & model_class            .~ Vehicle
  & model_str              .~ 7
  & model_tgh              .~ 6
  & model_wnd              .~ 10
  & model_att              .~ 3
  & model_quantumShielding .~ True
  & model_name             .~ "triarch stalker"

ghostArk :: Model
ghostArk = triarchStalker
  & model_ws               .~ 6
  & model_str              .~ 6
  & model_wnd              .~ 14
  & model_save             .~ 4
  & model_quantumShielding .~ True
  & model_name             .~ "ghost ark"

nightScythe :: Model
nightScythe = immortal
  & model_class                .~ Vehicle
  & model_ws                   .~ 6
  & model_str                  .~ 6
  & model_tgh                  .~ 6
  & model_wnd                  .~ 12
  & model_save                 .~ 3
  & model_rng_mods.mod_tobehit .~ -1
  & model_name                 .~ "night scythe"

doomScythe :: Model
doomScythe = nightScythe
  & model_name .~ "doom scythe"

destroyer :: Model
destroyer = immortal
  & model_tgh  .~ 5
  & model_wnd  .~ 3
  & model_att  .~ 2
  & model_name .~ "necron destroyer"

canoptekWraith :: Model
canoptekWraith = warrior
  & model_class .~ Beast
  & model_str   .~ 6
  & model_tgh   .~ 5
  & model_wnd   .~ 3
  & model_att   .~ 3
  & model_save  .~ 4
  & model_inv   .~ 3
  & model_name  .~ "canoptek wraith"

canoptekSpyder :: Model
canoptekSpyder = immortal
  & model_class .~ Monster
  & model_ws    .~ 4
  & model_bs    .~ 4
  & model_str   .~ 6
  & model_tgh   .~ 6
  & model_wnd   .~ 4
  & model_att   .~ 4
  & model_name  .~ "canoptek spyder"

monolith :: Model
monolith = immortal
  & model_class .~ Vehicle
  & model_ws    .~ 6
  & model_bs    .~ 3
  & model_str   .~ 8
  & model_tgh   .~ 8
  & model_wnd   .~ 20
  & model_att   .~ 3
  & model_name  .~ "necron monolith"

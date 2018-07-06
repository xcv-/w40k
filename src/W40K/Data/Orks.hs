module W40K.Data.Orks where

import Prelude hiding (Functor(..), Monad(..))
import Control.Lens

import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Core.Psychic

import W40K.Data.Common



gorkanaut :: Model
gorkanaut = rhino
  & model_ws   .~ 3
  & model_bs   .~ 5
  & model_str  .~ 8
  & model_tgh  .~ 8
  & model_wnd  .~ 18
  & model_att  .~ 6
  & model_ld   .~ 7
  & model_name .~ "gorkanaut"

klawOfGork_crush :: CCWeapon
klawOfGork_crush = basic_ccw
  & ccw_strMod .~ Times 2
  & ccw_ap     .~ -4
  & ccw_dmg    .~ d6
  & ccw_name   .~ "klaw of gork (or mork)"

module W40K.Data.ImperialKnights where

import Prelude hiding (Functor(..), Monad(..))

import Control.Lens
import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Data.Common


-- CC WEAPONS

-- titanicFeet :: CCWeapon
-- titanicFeet = basic_ccw
--   & ccw_attBonus .~ Times 3
--   & ccw_ap       .~ -2
--   & ccw_dmg      .~ d3
--
-- reaperChainsword :: CCWeapon
-- reaperChainsword = basic_ccw
--   & ccw_strMod   .~ Add 4
--   & ccw_ap       .~ -3
--   & ccw_dmg      .~ return 6
--
-- thunderstrikeGauntlet :: CCWeapon
-- thunderstrikeGauntlet = basic_ccw
--   & ccw_unwieldly .~ True
--   & ccw_strMod    .~ Times 2
--   & ccw_ap        .~ -4
--   & ccw_dmg       .~ return 6


-- RANGED WEAPONS

-- rapidFireBattleCannon :: RngWeapon
-- rapidFireBattleCannon = predatorAutocannon
--   & rw_shots  .~ twice d6
--   & rw_str    .~ 8
--   & rw_ap     .~ -2
--   & rw_dmg    .~ d3

-- EQUIPPED MODELS

-- knightPaladin :: CCWeapon -> RngWeapon -> EquippedModel
-- knightPaladin ccw rw = basicEquippedModel knightPaladin
--   & em_rw    .~ rw
--   & em_ccw   .~ ccw
--   & em_name  .~ "knight paladin's " ++ ccw^.ccw_name ++ "/" ++ rw^.rw_name

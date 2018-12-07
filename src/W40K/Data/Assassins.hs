{-# language RebindableSyntax #-}
module W40K.Data.Assassins where

import Prelude hiding (Functor(..), Applicative(..), Monad(..), liftA2, sequence, (=<<))

import W40K.Core.ConstrMonad
import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Data.Common

import Control.Lens

vindicareModel :: Model
vindicareModel = meq
  & model_ws   .~ 2
  & model_bs   .~ 2
  & model_wnd  .~ 5
  & model_att  .~ 5
  & model_ld   .~ 9
  & model_save .~ 6
  & model_inv  .~ 4
  & model_name .~ "vindicare assassin"

exitusRifleD3 :: Weapon
exitusRifleD3 = basicWeapon "exitus rifle (d3 damage)"
  & w_ap       .~ -3
  & w_dmg      .~ d3
  & w_noinv    .~ True
  & w_wounding .~ FixedWoundingAgainst [Infantry] 2

exitusRifleD6 :: Weapon
exitusRifleD6 = exitusRifleD3
  & w_dmg  .~ d6
  & w_name .~ "exitus rifle (d6 damage)"

exitusRifle :: RngWeapon
exitusRifle = null_rw
  & rw_shots                     .~ return 1
  & rw_str                       .~ 5
  & rw_class                     .~ Heavy
  & rw_weapon                    .~ exitusRifleD3
  & rw_weapon.w_hooks.hook_wound %~ addHook (MinModifiedRoll 6) (WoundHookModWeapon exitusRifleD6)
  & rw_name                      .~ "exitus rifle"

vindicare :: EquippedModel
vindicare = basicEquippedModel vindicareModel
  & em_rw .~ [exitusRifle]

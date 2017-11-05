{-# language RebindableSyntax #-}
module W40K.Data.Assassins where

import Prelude hiding (Functor(..), Applicative(..), Monad(..), liftA2, sequence, (=<<))

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

exitusRifleD3 :: Bool -> Weapon
exitusRifleD3 infantry = basicWeapon "exitus rifle (d3 damage)"
  & w_ap     .~ -3
  & w_dmg    .~ d3
  & w_noinv  .~ True
  & w_poison .~ if infantry then 2 else 0

exitusRifleD6 :: Bool -> Weapon
exitusRifleD6 infantry = exitusRifleD3 infantry
  & w_dmg  .~ d6
  & w_name .~ "exitus rifle (d6 damage)"

exitusRifle :: Bool -> RngWeapon
exitusRifle infantry = null_rw
  & rw_shots                     .~ return 1
  & rw_str                       .~ 5
  & rw_class                     .~ Heavy
  & rw_weapon                    .~ exitusRifleD3 infantry
  & rw_weapon.w_hooks.hook_wound .~ Just (RollHook 6 (WoundHookModWeapon (exitusRifleD6 infantry)))
  & rw_name                      .~ "exitus rifle"

vindicare :: Bool -> EquippedModel
vindicare infantry = basicEquippedModel vindicareModel
  & em_rw .~ exitusRifle infantry

vindicareRifleWounds :: Bool -> Model -> Prob Int
vindicareRifleWounds infantry tgt =
    numWounds Ranged [vindicare infantry] tgt

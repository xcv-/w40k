module Main where

import Prelude hiding (Functor(..), Monad(..))

import Control.Lens

import W40K.Core.Prob
import W40K.Core.Chart
import W40K.Core.Mechanics
import W40K.Core.Psychic

import W40K.Data.Common
import qualified W40K.Data.Assassins   as Assassins
import qualified W40K.Data.Eldar       as Eldar
import qualified W40K.Data.GreyKnights as GK
import qualified W40K.Data.Marines     as Marines
import qualified W40K.Data.Necrons     as Necrons
import qualified W40K.Data.Tau         as Tau
import qualified W40K.Data.Tyranids    as Tyranids

import GreyKnightTests

-- testAV ct srcs tgt = analyzeInt $ numWounds ct srcs tgt
-- --testAV ct srcs tgt = print $ probKill ct srcs 1 tgt
--
-- testAI ct srcs tgt = analyze $ numSlainModels ct srcs tgt
-- --testAI ct srcs tgt = analyzeInt $ numWounds ct srcs tgt
--
-- testPredatorAV :: Modifier -> Model -> IO ()
-- testPredatorAV modifier tgt = do
--     putStrLn "Predator 4 lascannons (wounds)"
--     testAV Ranged (modifier $ two (predator's lascannon) ++ [predator's (twin lascannon)])   tgt
--     putStrLn "Predator 2 lascannons + autocannon (wounds)"
--     testAV Ranged (modifier $ two (predator's lascannon) ++ [predator's predatorAutocannon]) tgt
--
-- testGodhammerAV :: Modifier -> Model -> IO ()
-- testGodhammerAV modifier tgt = do
--     putStrLn "LR lascannons (wounds)"
--     testAV Ranged (modifier $ two (landraider's (twin lascannon)))                                   tgt
--     putStrLn "LR lascannons + far melta (wounds)"
--     testAV Ranged (modifier $ two (landraider's (twin lascannon)) ++ [landraider's farMultiMelta])   tgt
--     putStrLn "LR lascannons + cc melta (wounds)"
--     testAV Ranged (modifier $ two (landraider's (twin lascannon)) ++ [landraider's closeMultiMelta]) tgt
--
-- testStormravenAV :: Modifier -> Model -> IO ()
-- testStormravenAV modifier tgt = do
--     putStrLn "SR typhoon + stormstrikes (wounds)"
--     testAV Ranged (modifier $ two (stormraven's stormstrike) ++ [stormraven's typhoonKrak])                                    tgt
--     putStrLn "SR lascannons + stormstrikes (wounds)"
--     testAV Ranged (modifier $ two (stormraven's stormstrike) ++ [stormraven's (twin lascannon)])                               tgt
--     putStrLn "SR lascannons + typhoon + stormstrikes (wounds)"
--     testAV Ranged (modifier $ two (stormraven's stormstrike) ++ [stormraven's typhoonKrak, stormraven's (twin lascannon)])     tgt
--     putStrLn "SR lascannons + far melta + stormstrikes (wounds)"
--     testAV Ranged (modifier $ two (stormraven's stormstrike) ++ [stormraven's (twin farMultiMelta), stormraven's (twin lascannon)])   tgt
--     putStrLn "SR lascannons + cc melta + stormstrikes (wounds)"
--     testAV Ranged (modifier $ two (stormraven's stormstrike) ++ [stormraven's (twin closeMultiMelta), stormraven's (twin lascannon)]) tgt
--     putStrLn "SR far melta (wounds)"
--     testAV Ranged (modifier $ [stormraven's (twin farMultiMelta)]) tgt
--     putStrLn "SR far melta + stormstrikes (wounds)"
--     testAV Ranged (modifier $ two (stormraven's stormstrike) ++ [stormraven's (twin farMultiMelta)]) tgt
--     putStrLn "SR cc melta (wounds)"
--     testAV Ranged (modifier $ [stormraven's (twin closeMultiMelta)]) tgt
--     putStrLn "SR cc melta + stormstrikes (wounds)"
--     testAV Ranged (modifier $ two (stormraven's stormstrike) ++ [stormraven's (twin closeMultiMelta)]) tgt
--
-- testHammerPaladinsAV :: Modifier -> Model -> IO ()
-- testHammerPaladinsAV modifier tgt = do
--     putStrLn "3 hammers"
--     testAV Melee (modifier $ paladinSquad 3 hammer)                         tgt
--
--     putStrLn "3 hammers with draigo aura"
--     testAV Melee (modifier $ within draigoAura $
--                                paladinSquad 3 hammer)                       tgt
--
--     putStrLn "3 hammers with banner"
--     testAV Melee (modifier $ with brotherhoodBanner $
--                                paladinSquad 3 hammer)                       tgt
--
--     putStrLn "3 hammers with banner and draigo aura"
--     testAV Melee (modifier $ with brotherhoodBanner $ within draigoAura $
--                                paladinSquad 3 hammer)                       tgt
--
--     putStrLn "3 hammers + grand master"
--     testAV Melee (modifier $ within grandMasterAura $
--                                grandMaster hammer : paladinSquad 3 hammer)  tgt
--
--     putStrLn "3 hammers + grand master with banner"
--     testAV Melee (modifier $ with brotherhoodBanner $ within grandMasterAura $
--                                grandMaster hammer : paladinSquad 3 hammer)  tgt
--
--     putStrLn "3 hammers + apothecary + grand master with banner"
--     testAV Melee (modifier $ with brotherhoodBanner $ within grandMasterAura $
--                                grandMaster hammer : gkApothecary hammer : paladinSquad 3 hammer)  tgt
--
-- genericTestAV :: Modifier -> Model -> IO ()
-- genericTestAV modifier tgt = do
--     putStrLn "\n++ Hammer paladins"
--     testHammerPaladinsAV modifier tgt
--
--     putStrLn "\n++ Ranged with no auras"
--     testStormravenAV modifier tgt
--     testGodhammerAV  modifier tgt
--
--     putStrLn "\n++ Ranged with grand master aura"
--     testStormravenAV (modifier . within grandMasterAura) tgt
--     testGodhammerAV  (modifier . within grandMasterAura) tgt
--
--     putStrLn "\n++ Ranged with draigo aura"
--     testStormravenAV (modifier . within draigoAura) tgt
--     testGodhammerAV  (modifier . within draigoAura) tgt
--
-- primarisTestAV :: IO ()
-- primarisTestAV = do
--     putStrLn "\n+++ vs Redemptor"
--     genericTestAV id redemptorDreadnought
--     putStrLn "\n+++ vs Land Raider"
--     genericTestAV id landraider
--
--
-- testGkRangedWeapons :: Modifier -> Model -> IO ()
-- testGkRangedWeapons modifier tgt = do
--     putStrLn "5 strikes rapid-firing bolters"
--     testAI Ranged (modifier $ rapidFiring $ strikeSquad 5 forceSword)                        tgt
--     putStrLn "5 strikes rapid-firing bolters and a psilencer"
--     testAI Ranged (modifier $ rapidFiring $ specialWeaponStrikeSquad5 psilencer forceSword)  tgt
--     putStrLn "5 strikes rapid-firing bolters and a psycannon"
--     testAI Ranged (modifier $ rapidFiring $ specialWeaponStrikeSquad5 psycannon forceSword)  tgt
--     putStrLn "10 strikes rapid-firing bolters"
--     testAI Ranged (modifier $ rapidFiring $ strikeSquad 10 forceSword)                       tgt
--     putStrLn "10 strikes rapid-firing bolters and a psilencer"
--     testAI Ranged (modifier $ rapidFiring $ specialWeaponStrikeSquad10 psilencer forceSword) tgt
--     putStrLn "10 strikes rapid-firing bolters and a psycannon"
--     testAI Ranged (modifier $ rapidFiring $ specialWeaponStrikeSquad10 psycannon forceSword) tgt
--     putStrLn "4 purgator psilencer + storm bolter"
--     testAI Ranged (modifier $ purgatorSquad psilencer)                                       tgt
--     putStrLn "4 purgator incinerator + storm bolter"
--     testAI Ranged (modifier $ purgatorSquad incinerator)                                     tgt
--     putStrLn "4 purgator psycannon + storm bolter"
--     testAI Ranged (modifier $ purgatorSquad psycannon)                                       tgt
--
-- testGkCcw :: Modifier -> Model -> IO ()
-- testGkCcw modifier tgt = do
--     putStrLn "5 strike swords"
--     testAI Melee (modifier $ strikeSquad 5 forceSword)   tgt
--     putStrLn "5 strike halberds"
--     testAI Melee (modifier $ strikeSquad 5 halberd)      tgt
--     putStrLn "5 strike falchions"
--     testAI Melee (modifier $ strikeSquad 5 twoFalchions) tgt
--     putStrLn "5 strike hammer"
--     testAI Melee (modifier $ strikeSquad 5 hammer)       tgt
--
-- testGktCcw :: Modifier -> Model -> IO ()
-- testGktCcw modifier tgt = do
--     putStrLn "5 GKT swords"
--     testAI Melee (modifier $ gkTerminatorSquad 5 forceSword)   tgt
--     putStrLn "5 GKT halberds"
--     testAI Melee (modifier $ gkTerminatorSquad 5 halberd)      tgt
--     putStrLn "5 GKT falchions"
--     testAI Melee (modifier $ gkTerminatorSquad 5 twoFalchions) tgt
--     putStrLn "5 GKT hammer"
--     testAI Melee (modifier $ gkTerminatorSquad 5 hammer)       tgt
--
-- testPaladinCcw :: Modifier -> Model -> IO ()
-- testPaladinCcw modifier tgt = do
--     putStrLn "3 paladin swords"
--     testAI Melee (modifier $ paladinSquad 3 forceSword)   tgt
--     putStrLn "3 paladin halberds"
--     testAI Melee (modifier $ paladinSquad 3 halberd)      tgt
--     putStrLn "3 paladin falchions"
--     testAI Melee (modifier $ paladinSquad 3 twoFalchions) tgt
--     putStrLn "3 paladin hammer"
--     testAI Melee (modifier $ paladinSquad 3 hammer)       tgt
--
-- genericInfantryRangedTest :: Modifier -> Model -> IO ()
-- genericInfantryRangedTest modifier tgt = do
--     putStrLn "\n++ Ranged moving with no auras"
--     testGkRangedWeapons modifier                                     tgt
--     putStrLn "\n++ Ranged moving with grand master"
--     testGkRangedWeapons (within grandMasterAura . moving . modifier) tgt
--     putStrLn "\n++ Ranged moving with draigo"
--     testGkRangedWeapons (within draigoAura . moving . modifier)      tgt
--
--     putStrLn "\n++ Ranged moving with no auras and psybolt ammo"
--     testGkRangedWeapons (with psyboltAmmo . moving . modifier)                          tgt
--     putStrLn "\n++ Ranged moving with grand master and psybolt ammo"
--     testGkRangedWeapons (with psyboltAmmo . within grandMasterAura . moving . modifier) tgt
--     putStrLn "\n++ Ranged moving with draigo and psybolt ammo"
--     testGkRangedWeapons (with psyboltAmmo . within draigoAura . moving . modifier)      tgt
--
--     putStrLn "\n++ Ranged moving with no auras and psychic onslaught"
--     testGkRangedWeapons (with psyOnslaughtAmmo . moving . modifier)                          tgt
--     putStrLn "\n++ Ranged moving with grand master and psychic onslaught"
--     testGkRangedWeapons (with psyOnslaughtAmmo . within grandMasterAura . moving . modifier) tgt
--     putStrLn "\n++ Ranged moving with draigo and psychic onslaught"
--     testGkRangedWeapons (with psyOnslaughtAmmo . within draigoAura . moving . modifier)      tgt
--
-- genericInfantryCcwTest :: Modifier -> Model -> IO ()
-- genericInfantryCcwTest modifier tgt = do
--     putStrLn "\n++ CCW with banner"
--     testGkCcw (with brotherhoodBanner . modifier) tgt
--     testGktCcw (with brotherhoodBanner . modifier) tgt
--     testPaladinCcw (with brotherhoodBanner . modifier) tgt
--
--     putStrLn "\n++ CCW with grand master aura"
--     testGkCcw (within grandMasterAura . modifier) tgt
--     testGktCcw (within grandMasterAura . modifier) tgt
--     testPaladinCcw (within grandMasterAura . modifier) tgt
--
--     putStrLn "\n++ CCW with draigo aura"
--     testGkCcw (within draigoAura . modifier) tgt
--     testGktCcw (within draigoAura . modifier) tgt
--     testPaladinCcw (within draigoAura . modifier) tgt
--
-- genericInfantryTest :: Modifier -> Model -> IO ()
-- genericInfantryTest modifier tgt = do
--     genericInfantryRangedTest modifier tgt
--     genericInfantryCcwTest modifier tgt

main = do
    -- analyze $ numSlainModels Ranged (rapidFiring $ map (em_rw %~ psybolt)
    --                                              $ strikeSquad 10 twoFalchions)
    --                                 primaris

    -- genericTestAV id Eldar.waveSerpent
    -- putStrLn "\n++ vs Primaris"
    -- genericInfantryRangedTest (within grandMasterAura) Eldar.wraithguard

    -- let termies = gkTerminatorSquad 5 hammer
    --     pallies = paladinSquad 3 hammer
    --     apothecary = gkApothecary hammer
    --     banner = brotherhoodAncient
    --     purifiers = specialWeaponPurifierSquad5 psilencer twoFalchions

    -- putStrLn "\n++ vs Predator"

    -- putStrLn "\n+ Psychic mortal wounds"
    -- analyzeInt $ castPsychics [ (gkCasting False [draigo]             ritesOfBanishment, Nothing)
    --                           , (gkCasting True  [draigo]             vortexOfDoom,      Nothing)
    --                           , (gkCasting False termies              ritesOfBanishment, Nothing)
    --                           , (gkCasting False [brotherhoodAncient] cleansingFlame,    Nothing)
    --                           , (gkCasting False purifiers            cleansingFlame,    Nothing)
    --                           , (gkCasting False [apothecary]         ritesOfBanishment, Nothing)
    --                           ]
    --                           predator

    -- putStrLn "\n+ Draigo"
    -- print $ probKill Melee (within draigoAura $
    --                                 with brotherhoodBanner $
    --                                   [draigo])
    --                              1 predator

    -- putStrLn "\n+ Termies"
    -- print $ probKill Melee (within draigoAura $
    --                                 with brotherhoodBanner $
    --                                   termies)
    --                              1 predator

    -- putStrLn "\n++ vs Landraider"

    -- putStrLn "\n+ Psychic mortal wounds"
    -- analyzeInt $ castPsychics [ (gkCasting False [draigo]             ritesOfBanishment, Nothing)
    --                           , (gkCasting False termies              ritesOfBanishment, Nothing)
    --                           , (gkCasting False [brotherhoodAncient] cleansingFlame,    Nothing)
    --                           , (gkCasting False [apothecary]         ritesOfBanishment, Nothing)
    --                           , (gkCasting True  [draigo]             vortexOfDoom,      Nothing)
    --                           ]
    --                           landraider

    -- putStrLn "\n+ Draigo"
    -- print $ probKill Melee (within draigoAura $
    --                                 with brotherhoodBanner $
    --                                   [draigo])
    --                              1 landraider

    -- putStrLn "\n+ Termies"
    -- analyzeInt $ numWounds Melee (within draigoAura $
    --                                 with brotherhoodBanner $
    --                                   hammerhanded termies)
    --                              landraider

    -- putStrLn "\n++ vs Knight paladin"

    -- let dup a = a ++ a

    -- putStrLn "\n+ Land raider (far)"
    -- analyzeInt $ numWounds Ranged (within draigoAura $
    --                                  dup (landraider's farMultiMelta : two (landraider's (twin lascannon))))
    --                               knightPaladinRanged

    let inCover = (model_save -~ 1) . (model_name <>~ " in cover")


    --mainAnalysis [ (ByTarget, ("# wounds", numWounds), godhammerAvTest,  [Eldar.waveSerpent])
    --             , (ByTarget, ("# wounds", numWounds), stormravenAvTest, [Eldar.waveSerpent])
    --             , (ByTarget, ("# wounds", numWounds), andAuras [ ("AV SR + laser razor", Ranged, [Marines.stormraven's (twin closeMultiMelta)] ++ two (Marines.stormraven's lascannon) ++ two (Marines.stormraven's stormstrike) ++ [Marines.razorback's (twin lascannon)])
    --                                                   ],                [Eldar.waveSerpent])
                 -- , (ByTarget, ("# wounds", numWounds), terminatorAvTest, [Eldar.waveSerpent])
                 -- , (ByTarget, ("# wounds", numWounds), paladinAvTest,    [Eldar.waveSerpent])
    --           ]

    -- mainAnalysis [ (ByTarget, ("slain models", numSlainModelsInt), smallStrikeRngInfTest, [meq, Marines.primaris, Marines.gravis])
    --              , (ByTarget, ("slain models", numSlainModelsInt), purifierRngInfTest,    [meq, Marines.primaris, Marines.gravis])
    --              , (ByTarget, ("slain models", numSlainModelsInt), purgatorRngInfTest,    [meq, Marines.primaris, Marines.gravis])
    --              , (ByTarget, ("slain models", numSlainModelsInt), smallPagkCcInfTest,    [meq, Marines.primaris, Marines.gravis])
    --              , (ByTarget, ("slain models", numSlainModelsInt), smallTagkCcInfTest,    [meq, Marines.primaris, Marines.gravis])
    --              , (ByTarget, ("slain models", numSlainModelsInt), smallPaladinCcInfTest, [meq, Marines.primaris, Marines.gravis])
    --              , (ByTarget, ("slain models", numSlainModelsInt), largePagkRngInfTest,   [meq, Marines.primaris, Marines.gravis])
    --              ]

    let hardToHit = model_mods.mod_tobehit -~ 1

    let rerolling1's = em_model.model_mods.mod_rrtohit <>~ RerollOnes

    let cacahuete = basicEquippedModel Eldar.wraithguard & em_rw .~ Eldar.wraithcannon

    print $ probKill Ranged (replicate 5 (rerolling1's cacahuete)) 3 (hardToHit Tyranids.hiveGuard)

    mainWithPlot
        [ ("wraithguard",
            [ ("vs hard to hit hive guard", numSlainModels Ranged (replicate 5 cacahuete) (hardToHit Tyranids.hiveGuard))
            , ("vs hive guard",             numSlainModels Ranged (replicate 5 cacahuete) Tyranids.hiveGuard)
            ]
          )
        , ("wraithguard (rerolling 1's)",
            [ ("vs hard to hit hive guard", numSlainModels Ranged (replicate 5 (rerolling1's cacahuete)) (hardToHit Tyranids.hiveGuard))
            , ("vs hive guard",             numSlainModels Ranged (replicate 5 (rerolling1's cacahuete)) Tyranids.hiveGuard)
            ]
          )
        ]

    -- mainWithPlot
    --     [("vindicare",
    --         [ ("vs girlyman", Assassins.vindicareRifleWounds False Marines.guilliman)
    --         ])
    --     ]

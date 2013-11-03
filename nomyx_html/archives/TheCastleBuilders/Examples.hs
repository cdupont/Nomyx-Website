{-# LANGUAGE TupleSections, GADTs, EmptyDataDecls, TypeFamilies, DeriveDataTypeable,
    ScopedTypeVariables, TypeSynonymInstances #-}

-- | This file gives a list of example rules that the players can submit.
--You can copy-paste them in the field "Code" of the web GUI.
--Don't hesitate to get inspiration from there and create your own rules!
module Language.Nomyx.Examples(nothing, helloWorld, accounts, createBankAccount, winXEcuPerDay,
    winXEcuOnRuleAccepted, moneyTransfer, delRule, voteWithMajority, king, makeKing, monarchy,
    revolution, victoryXRules, victoryXEcu, displayTime, noGroupVictory, iWin, returnToDemocracy,
    banPlayer, referendum, referendumOnKickPlayer, gameMasterElections, gameMaster, bravoButton,
    enterHaiku, displayBankAccount,
    module Data.Time.Recurrence, module Control.Monad, module Data.List, module Data.Time.Clock) where

import Language.Nomyx.Definition
import Language.Nomyx.Rule
import Language.Nomyx.Expression
import Language.Nomyx.Vote
import Data.Function
import Data.Time.Clock hiding (getCurrentTime)
import Data.Time.Recurrence hiding (filter)
import Control.Arrow
import Data.List
import Control.Monad
import Language.Nomyx.Utils (oneDay)
import Safe (readDef)

-- | A rule that does nothing
nothing :: RuleFunc
nothing = return Void

-- | A rule that says hello to all players
helloWorld :: RuleFunc
helloWorld = voidRule $ outputAll "hello, world!"

-- | account variable name and type
accounts :: MsgVar [(PlayerNumber, Int)]
accounts = msgVar "Accounts"

-- | Create a bank account for each players
createBankAccount :: RuleFunc
createBankAccount = voidRule $ createValueForEachPlayer_ accounts

-- | Permanently display the bank accounts
displayBankAccount :: RuleFunc
displayBankAccount = voidRule $ forEachPlayer_ displayPlayerAccount

-- | each player wins X Ecu each day
-- you can also try with "minutly" or "monthly" instead of "daily" and everything in the "time-recurrence" package
winXEcuPerDay :: Int -> RuleFunc
winXEcuPerDay x = voidRule $ schedule_ (recur daily) $ modifyAllValues accounts (+x)

-- | a player wins X Ecu if a rule proposed is accepted
winXEcuOnRuleAccepted :: Int -> RuleFunc
winXEcuOnRuleAccepted x = voidRule $ onEvent_ (RuleEv Activated) $ \(RuleData rule) -> modifyValueOfPlayer (_rProposedBy rule) accounts (+x)

-- | a player can transfer money to another player
-- it does not accept new players or check if balance is positive, to keep the example simple
moneyTransfer :: RuleFunc
moneyTransfer = voidRule $ do
    pls <- getAllPlayerNumbers
    when (length pls >= 2) $ forEachPlayer_ (selPlayer pls) where
       selPlayer pls src = onInputRadio_ "Transfer money to player: " (delete src $ sort pls) (selAmount src) src
       selAmount src dst = onInputTextOnce_ ("Select Amount to transfert to player: " ++ show dst) (transfer src dst) src
       transfer src dst amount = do
           modifyValueOfPlayer dst accounts (\a -> a + (readDef 0 amount))
           modifyValueOfPlayer src accounts (\a -> a - (readDef 0 amount))
           newOutput_ ("You gave " ++ amount ++ " ecus to player " ++ show dst) src
           newOutput_ ("Player " ++ show src ++ " gaved you " ++ amount ++ "ecus") dst


-- | delete a rule
delRule :: RuleNumber -> RuleFunc
delRule rn = voidRule $ suppressRule rn >> autoDelete

-- | player pn is the king: we create a variable King to identify him,
-- and we prefix his name with "King"
makeKing :: PlayerNumber -> RuleFunc
makeKing pn = voidRule $ do
   voidRule $ newVar_ "King" pn
   modifyPlayerName pn ("King " ++)

king :: V PlayerNumber
king = V "King"

-- | Monarchy: only the king decides which rules to accept or reject
monarchy :: RuleFunc
monarchy = voidRule $ onEvent_ (RuleEv Proposed) $ \(RuleData rule) -> do
    k <- readVar_ king
    onInputRadioEnumOnce_ ("Your Royal Highness, do you accept rule " ++ (show $ _rNumber rule) ++ "?") True (activateOrReject rule) k


-- | Revolution! Hail to the king!
-- This rule suppresses the democracy (usually rules 1 and 2), installs the king and activates monarchy.
revolution :: PlayerNumber -> RuleFunc
revolution player = voidRule $ do
    suppressRule 1
    makeKing player
    rNum <- addRuleParams "Monarchy" monarchy "monarchy" "Monarchy: only the king can vote on new rules"
    activateRule_ rNum
    --autoDelete

-- | set the victory for players having more than X accepted rules
victoryXRules :: Int -> RuleFunc
victoryXRules x = voidRule $ onEvent_ (RuleEv Activated) $ \_ -> do
    rs <- getActiveRules
    let counts = map (_rProposedBy . head &&& length) $ groupBy ((==) `on` _rProposedBy) rs
    let victorious = map fst $ filter ((>= x) . snd) counts
    when (length victorious /= 0) $ setVictory victorious

victoryXEcu :: Int -> RuleFunc
victoryXEcu x = voidRule $ onEvent_ (RuleEv Activated) $ \_ -> do
    as <- readMsgVar_ accounts
    let victorious = map fst $ filter ((>= x) . snd) as
    if (length victorious /= 0) then setVictory victorious else return ()

-- | will display the time to all players in 5 seconds
displayTime :: RuleFunc
displayTime = voidRule $ do
    t <- getCurrentTime
    onEventOnce_ (Time $ addUTCTime 5 t) $ \(TimeData t) -> outputAll $ show t

-- | Only one player can achieve victory: No group victory.
-- Forbidding group victory usually becomes necessary when lowering the voting quorum:
-- a coalition of players could simply force a "victory" rule and win the game.
noGroupVictory ::  RuleFunc
noGroupVictory = voidRule $ onEvent_ Victory $ \(VictoryData ps) -> when (length ps >1) $ setVictory []

-- | Rule that state that you win. Good luck on having this accepted by other players ;)
iWin :: RuleFunc
iWin = voidRule $ getSelfProposedByPlayer >>= giveVictory


-- | a majority vote, with the folowing parameters:
-- a quorum of 2 voters is necessary for the validity of the vote
-- the vote is assessed after every vote in case the winner is already known
-- the vote will finish anyway after one day
voteWithMajority :: RuleFunc
voteWithMajority = onRuleProposed $ voteWith_ (majority `withQuorum` 2) $ assessOnEveryVote >> assessOnTimeDelay oneDay

-- | Change current system (the rules passed in parameter) to absolute majority (half participants plus one)
returnToDemocracy :: [RuleNumber] -> RuleFunc
returnToDemocracy rs = voidRule $ do
   mapM_ suppressRule rs
   rNum <- addRuleParams "vote with majority" voteWithMajority "voteWithMajority" "majority with a quorum of 2"
   activateRule_ rNum
   autoDelete

-- | kick a player and prevent him from returning
banPlayer :: PlayerNumber -> RuleFunc
banPlayer pn = voidRule $ do
   delPlayer pn
   onEvent_ (Player Arrive) $ \(PlayerData _) -> void $ delPlayer pn

-- * Referendum & elections

-- | triggers a referendum, if the outcome is yes player 2 will be kicked
referendumOnKickPlayer :: RuleFunc
referendumOnKickPlayer = referendum " kick player 2" (void $ delPlayer 2)

-- | triggers elections (all players are candidates), the winner becomes game master
gameMasterElections :: RuleFunc
gameMasterElections = voidRule $ do
   pls <- getPlayers
   elections "Game Master" pls makeGM

makeGM :: PlayerNumber -> Nomex()
makeGM pn = do
   newVar "GameMaster" pn
   void $ modifyPlayerName pn ("GameMaster " ++)

gameMaster :: V PlayerNumber
gameMaster = V "GameMaster"

-- | display a button and greets you when pressed (for player 1)
bravoButton :: RuleFunc
bravoButton = voidRule $ voidRule $ onInputButton_ "Click here:" (const $ outputAll "Bravo!") 1

enterHaiku :: RuleFunc
enterHaiku = voidRule $ onInputTextarea_ "Enter a haiku:" outputAll 1

displayPlayerAccount :: PlayerNumber -> Nomex ()
displayPlayerAccount pn = do
   sp <- showPlayer
   displayVar pn accounts (\l -> "Accounts:\n" ++ concatMap (\(i,a) -> (sp i) ++ "\t" ++ (show a) ++ "\n") l)

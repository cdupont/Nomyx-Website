{-# LANGUAGE DeriveDataTypeable, GADTs, ScopedTypeVariables, TupleSections,
    FlexibleInstances, TypeFamilies, FlexibleContexts, Rank2Types #-}

-- | Basic rules examples.
module Language.Nomyx.Rule where

import Prelude hiding (foldr)
import Language.Nomyx.Expression
import Language.Nomyx.Definition
import Control.Arrow
import Data.Lens
import Data.Maybe
import Data.List
import Control.Applicative


-- | This rule will activate automatically any new rule.
autoActivate :: RuleFunc
autoActivate = voidRule $ onEvent_ (RuleEv Proposed) (activateRule_ . _rNumber . ruleData)

-- | This rule will forbid any new rule to delete the rule in parameter
--immutableRule :: RuleNumber -> RuleFunc
--immutableRule rn = return $ Meta f where
--   f r = do
--      protectedRule <- getRule rn
--      case protectedRule of
--         Just pr -> case _rRuleFunc r of
--            RuleRule paramRule -> paramRule pr
--            _ -> return $ BoolResp True
--         Nothing -> return $ BoolResp True

-- | A rule will be always legal
legal :: RuleFunc
legal =  return $ Meta (\_ -> return $ BoolResp True)

-- | A rule will be always illegal
illegal :: RuleFunc
illegal = return $ Meta (\_ -> return $ BoolResp False)

-- | active metarules are automatically used to evaluate a given rule
--checkWithMetarules :: Rule -> Nomex (Event (Message ForAgainst)
--checkWithMetarules rule = do
--    rs <- getActiveRules
--    (metas :: [Rule -> Nomex BoolResp]) <- mapMaybeM maybeMetaRule rs
--    let (evals :: [Nomex BoolResp]) = map (\meta -> meta rule) metas
--    foldr (&&*) true evals


maybeMetaRule :: Rule -> Nomex (Maybe (Rule -> Nomex BoolResp))
maybeMetaRule Rule {_rRuleFunc = rule} = do
   meta <- rule
   case meta of
      (Meta m) -> return $ Just m
      _ -> return Nothing




-- | activate or reject a rule
activateOrReject :: Rule -> Bool -> Nomex ()
activateOrReject r b = if b then activateRule_ (_rNumber r) else rejectRule_ (_rNumber r)

-- | perform an action for each current players, new players and leaving players
forEachPlayer :: (PlayerNumber -> Nomex ()) -> (PlayerNumber -> Nomex ()) -> (PlayerNumber -> Nomex ()) -> Nomex ()
forEachPlayer action actionWhenArrive actionWhenLeave = do
    pns <- getAllPlayerNumbers
    mapM_ action pns
    onEvent_ (Player Arrive) $ \(PlayerData p) -> actionWhenArrive $ _playerNumber p
    onEvent_ (Player Leave) $ \(PlayerData p) -> actionWhenLeave $ _playerNumber p

-- | perform the same action for each players, including new players
forEachPlayer_ :: (PlayerNumber -> Nomex ()) -> Nomex ()
forEachPlayer_ action = forEachPlayer action action (\_ -> return ())


-- | create a value initialized for each players
--manages players joining and leaving
createValueForEachPlayer :: Int -> MsgVar [(Int, Int)] -> Nomex ()
createValueForEachPlayer initialValue mv = do
    pns <- getAllPlayerNumbers
    v <- newMsgVar_ (getMsgVarName mv) $ map (,initialValue::Int) pns
    forEachPlayer (const $ return ())
                  (\p -> modifyMsgVar v ((p, initialValue) : ))
                  (\p -> modifyMsgVar v $ filter $ (/= p) . fst)

-- | create a value initialized for each players initialized to zero
--manages players joining and leaving
createValueForEachPlayer_ :: MsgVar [(Int, Int)] -> Nomex ()
createValueForEachPlayer_ = createValueForEachPlayer 0

getValueOfPlayer :: PlayerNumber -> MsgVar [(Int, Int)] -> Nomex (Maybe Int)
getValueOfPlayer pn var = do
   value <- readMsgVar_ var
   return $ lookup pn value

modifyValueOfPlayer :: PlayerNumber -> MsgVar [(Int, Int)] -> (Int -> Int) -> Nomex ()
modifyValueOfPlayer pn var f = modifyMsgVar var $ map $ (\(a,b) -> if a == pn then (a, f b) else (a,b))

modifyAllValues :: MsgVar [(Int, Int)] -> (Int -> Int) -> Nomex ()
modifyAllValues var f = modifyMsgVar var $ map $ second f

-- | Player p cannot propose anymore rules
noPlayPlayer :: PlayerNumber -> RuleFunc
noPlayPlayer p = return $ Meta $ \r -> return $ BoolResp $ (_rProposedBy r) /= p

-- | a rule can autodelete itself (generaly after having performed some actions)
autoDelete :: Nomex ()
autoDelete = getSelfRuleNumber >>= suppressRule_


-- | All rules from player p are erased:
eraseAllRules :: PlayerNumber -> Nomex Bool
eraseAllRules p = do
    rs <- getRules
    let myrs = filter ((== p) . getL rProposedBy) rs
    res <- mapM (suppressRule . _rNumber) myrs
    return $ and res

showPlayer :: Nomex (PlayerNumber -> String)
showPlayer = do
   pls <- getPlayers
   let showP pn = fromMaybe ("Player " ++ (show pn)) $ _playerName <$> find (\(PlayerInfo mypn _) -> mypn == pn) pls
   return showP   

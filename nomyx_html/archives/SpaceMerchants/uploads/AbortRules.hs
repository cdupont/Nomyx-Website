module AbortRules where

import Prelude
import Control.Monad
import Language.Nomyx

getPendingRulesByPlayer :: PlayerNumber -> NomexNE [RuleInfo]
getPendingRulesByPlayer pn = do
  allRules <- getRules
  return [rule | rule <- allRules
               , _rStatus rule == Pending
               , _rProposedBy rule == pn]

ruleDeletetionEventForPlayer :: PlayerNumber -> Event RuleNumber
ruleDeletetionEventForPlayer pn = do
  pendingRules <- liftEvent $ getPendingRulesByPlayer pn
  let options = [(_rNumber rule, _rName rule) | rule <- pendingRules]
  inputRadio pn "Reject this rule: " options

allowPendingRuleRejectionByAuthor :: Nomex ()
allowPendingRuleRejectionByAuthor = 
  void $ forEachPlayer_ $ \pn ->
    void $ onEvent_ (ruleDeletetionEventForPlayer pn) rejectRule_ 


-- | Voting system with messages broadcasted to signal the begining and end of a vote
module Vote2 where

import Prelude hiding (foldr)
import Language.Nomyx
import Data.Typeable
import Data.Maybe
import Data.Time hiding (getCurrentTime)
import Control.Monad
import Control.Applicative
import System.Locale

-- | information broadcasted when a vote begins
data VoteBegin = VoteBegin { endAt :: UTCTime,
                             beginVoteRN :: RuleNumber,
                             voteEvent :: EventNumber }
                             deriving (Show, Eq, Ord, Typeable)

-- | information broadcasted when a vote ends
data VoteEnd = VoteEnd { endVoteRN :: RuleNumber,
                         votes :: [(PlayerNumber, Maybe Bool)],
                         passed :: Bool,
                         finishedAt :: UTCTime}
                         deriving (Show, Eq, Ord, Typeable)

-- | message sent when a vote begins
voteBegin :: Msg VoteBegin
voteBegin = Msg "VoteBegin"

-- | message sent when a vote ends
voteEnd :: Msg VoteEnd
voteEnd = Msg "VoteEnd"

-- | vote at unanimity every incoming rule
unanimityVote :: Nomex ()
unanimityVote = do
   onRuleProposed $ Vote2.callVoteRule unanimity oneDay
   displayVotes

-- | call a vote on a rule for every players, with an assessing function and a delay
callVoteRule :: AssessFunction -> NominalDiffTime -> RuleInfo -> Nomex ()
callVoteRule assess delay ri = do
   endTime <- addUTCTime delay <$> liftEffect getCurrentTime
   Vote2.callVoteRule' assess endTime ri

callVoteRule' :: AssessFunction -> UTCTime -> RuleInfo -> Nomex ()
callVoteRule' assess endTime ri = Vote2.callVote assess endTime (_rName ri) (_rNumber ri) (Vote2.finishVote assess ri)

-- | actions to do when the vote is finished
finishVote :: AssessFunction -> RuleInfo -> [(PlayerNumber, Maybe Bool)] -> Nomex ()
finishVote assess ri vs = do
   let passed = fromJust $ assess $ getVoteStats (map snd vs) True
   activateOrRejectRule ri passed
   end <- liftEffect getCurrentTime
   sendMessage voteEnd (VoteEnd (_rNumber ri) vs passed end)

-- | call a vote for every players, with an assessing function, a delay and a function to run on the result
callVote :: AssessFunction -> UTCTime -> String -> RuleNumber -> ([(PlayerNumber, Maybe Bool)] -> Nomex ()) -> Nomex ()
callVote assess endTime name rn payload = do
   let title = "Vote for rule: \"" ++ name ++ "\" (#" ++ (show rn) ++ "):"
   en <- onEventOnce (voteWith endTime assess title) payload
   sendMessage voteBegin (VoteBegin endTime rn en)

-- | display the votes (ongoing and finished)
displayVotes :: Nomex ()
displayVotes = do
   void $ onMessage voteEnd displayFinishedVote
   void $ onMessage voteBegin displayOnGoingVote

-- | display an on going vote
displayOnGoingVote :: VoteBegin -> Nomex ()
displayOnGoingVote (VoteBegin end rn en) = void $ outputAll $ do
   mds <- getIntermediateResults en
   let mbs = map getBooleanResult <$> mds
   pns <- getAllPlayerNumbers
   case mbs of
      Just bs -> showOnGoingVote (getVotes pns bs) rn
      Nothing -> return ""

-- | display a finished vote
displayFinishedVote :: VoteEnd -> Nomex ()
displayFinishedVote (VoteEnd rn vs passed _) = void $ outputAll $ showFinishedVote rn passed vs


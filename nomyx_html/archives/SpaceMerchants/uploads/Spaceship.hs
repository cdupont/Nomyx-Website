
module Spaceship where

import Prelude
import Language.Nomyx
import Control.Monad
import Data.Typeable

data SpaceshipPart = 
   SpaceshipPart { partId         :: Integer,        -- unique integer to identify this part in an spaceship
                   partType       :: String,         -- name of part: Cockpit, engine, fuel tank...
                   partPrice      :: Int,            -- cost in blings
                   partConnectors :: PartConnectors} -- available connections with other parts
                   deriving (Show, Eq, Ord, Typeable)

--connectors available in a part                   
data PartConnectors = 
   PartConnectors { up    :: Bool,
                    down  :: Bool,
                    right :: Bool,
                    left  :: Bool}
                    deriving (Show, Eq, Ord, Typeable)

data ConnectionOrientation = Above | Underneath | RightOf | LeftOf
                             deriving (Show, Eq)

data PartConnection = 
   PartConnection { sourcePartId        :: Integer,               -- this part...
                    connectedWithPartId :: Integer,               -- is connected with this part...
                    connection          :: ConnectionOrientation} -- with this orientation
                    deriving (Show, Eq, Typeable)

-- a spaceship is a list of parts, with their interconnections.
-- the player has to decide how to connect them, for better efficiency...
data Spaceship = Spaceship { parts       :: [SpaceshipPart],
                             connections :: [PartConnection]}
                             deriving (Show, Eq, Typeable)

--definition of a cockpit.
--it should be located at the front of the spaceship (on the right).
cockpit :: SpaceshipPart
cockpit = SpaceshipPart { partId = 1,
                          partType = "cockpit",
                          partPrice = 500,
                          partConnectors = PartConnectors {up = False, down = False, right = False, left = True}}

--a minimal spaceship
defaultSpaceship :: Spaceship
defaultSpaceship = Spaceship { parts = [cockpit], connections = []}

-- | variable storing the spaceship of each player
spaceships :: MsgVar [(PlayerNumber, Spaceship)]
spaceships = msgVar "spaceships"

-- | Create a spaceship for each players
createSpaceships :: Rule
createSpaceships = void $ createValueForEachPlayer defaultSpaceship spaceships 


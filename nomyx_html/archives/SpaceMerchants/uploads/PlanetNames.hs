{-# LANGUAGE OverloadedStrings #-}
module PlanetNames
  ( genName
  ) where

import Prelude

import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Text as T

import Language.Nomyx

-- Generate a name by randomly selecting 1 - 4 syllables from a pool of 50.
genName :: Nomex T.Text
genName = do
    num <- genSyllables
    T.toTitle . T.concat <$> replicateM num genSyllable

-- Generate a single syllable
genSyllable :: Nomex T.Text
genSyllable = genericIndex syllables <$> getRandomNumber (0, 49 :: Int)

-- Roll a D10. One syllable on a 1, 2 syllables on a 2-6, 3 syllables on a 7-9, 4 otherwise
genSyllables :: Nomex Int
genSyllables = numForDie <$> getRandomNumber (1, 10)
  where
    numForDie :: Int -> Int
    numForDie x | x < 2 = 1
                | x < 7 = 2
                | x < 10 = 3
                | otherwise = 4

-- These were taken from a list of well-known systems from popular science fiction movies.
syllables :: [T.Text]
syllables =
    [ "oph", "eri", "iuchi", "dani", "urs"
    , "cyg", "aqua", "rii", "pis", "cium"
    , "tau", "leo", "nis", "ri", "per"
    , "sei", "gem", "min", "or", "rum"
    , "boot", "o", "rio", "nis", "tol"
    , "men", "kab", "rus", "al", "phe"
    , "kka", "dra", "co", "nis", "bor"
    , "thu", "ban", "lae", "scor", "pii"
    , "sad", "als", "uud", "ven", "nat"
    , "car", "cyg", "gni", "corn", "cap"
    ]

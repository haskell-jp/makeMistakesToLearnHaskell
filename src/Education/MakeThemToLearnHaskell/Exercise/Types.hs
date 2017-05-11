{-# OPTIONS_GHC -Wno-unused-imports #-}

module Education.MakeThemToLearnHaskell.Exercise.Types where

#include <imports/external.hs>


data Exercise =
  Exercise
    { exerciseName :: !String
    , verify :: String -> IO Result
    }


data Result =
  Error !Details | Fail !Details | Success !Details deriving (Eq, Show)

data Record =
  Record
    { lastShownId :: !ExerciseId
    } deriving Generic

instance Yaml.FromJSON Record
instance Yaml.ToJSON Record

type Details = Text

type ExerciseId = Int

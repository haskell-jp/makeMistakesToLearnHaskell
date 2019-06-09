{-# LANGUAGE TemplateHaskell #-}

module Education.MakeMistakesToLearnHaskell.Commons.Exercise where

import qualified Data.Aeson.TH  as AT
import           Data.Text.Lazy (Text)


type Details = Text

type SourceCode = Text

type Name = String


data FailBy =
    WrongOutput !Details
  | CommandFailed
      !FilePath   -- ^ Command name
      !Details -- ^ Output by command
      !Details -- ^ Diagnosis message
  deriving (Eq, Show)

$(AT.deriveJSON AT.defaultOptions ''FailBy)

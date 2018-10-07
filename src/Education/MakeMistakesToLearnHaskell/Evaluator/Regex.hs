{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Thin wrapper around @Text.Regex.Applicative@
--   useful for evaluating source code input by the user.

module Education.MakeMistakesToLearnHaskell.Evaluator.Regex
  ( matchSubstring
  , matchesSubstring
  ) where


#include <imports/external.hs>


matchSubstring :: Regex.RE s a -> [s] -> Maybe a
matchSubstring re = Regex.match (Regex.few Regex.anySym *> re)


matchesSubstring :: Regex.RE s a -> [s] -> Bool
matchesSubstring re = isJust . matchSubstring re

{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Thin wrapper around @Text.Regex.Applicative@
--   useful for evaluating source code input by the user.

module Education.MakeMistakesToLearnHaskell.Evaluator.Regex
  ( GhcToken
  , matchSub
  , matchesSub
  , dropUntilFirst

  , singleArgFunApp
  ) where


#include <imports/external.hs>

import           Education.MakeMistakesToLearnHaskell.Evaluator.Types

type GhcToken = (GHC.Token, TextS.Text)


matchSub :: Regex.RE s a -> [s] -> Maybe a
matchSub re =
  Regex.match (Regex.few Regex.anySym *> re <* Regex.few Regex.anySym)


matchesSub :: Regex.RE s a -> [s] -> Bool
matchesSub re = isJust . matchSub re


dropUntilFirst :: Regex.RE s a -> [s] -> [s]
dropUntilFirst re xs =
  maybe xs (\(_before, _matched, after) -> after) $ Regex.findFirstInfix re xs


-- NOTE: Depth seeems required due to the limitation of regex-applicative,
--       which has to scan all regex before executing.
singleArgFunApp :: Natural -> Regex.RE GhcToken SingleArgFunApp
singleArgFunApp depth =
  insideParens
    $   SingleArgFunApp
    <$> identifier
    <*  skipSpace
    <*> (if depth == 0 then pure Nothing else optional $ singleArgFunApp $ depth - 1)

  where
    symbol :: TextS.Text -> Regex.RE GhcToken ()
    symbol t = void $ Regex.sym (GHC.SymbolTok, t)

    hasSympol :: TextS.Text -> Regex.RE GhcToken Bool
    hasSympol t = (symbol t $> True) <|> pure False

    identifier :: Regex.RE GhcToken TextS.Text
    identifier = snd <$> Regex.psym ((== GHC.VariableTok) . fst)

    skipSpace :: Regex.RE GhcToken ()
    skipSpace = void $ optional (Regex.psym ((== GHC.SpaceTok) . fst))

    insideParens :: Regex.RE GhcToken (HasParens -> a) -> Regex.RE GhcToken a
    insideParens re =
      (symbol "(" *> skipSpace *> re <*> (toHasParens <$> (skipSpace *> hasSympol ")"))) <|> (re <*> pure NoParens)

    toHasParens :: Bool -> HasParens
    toHasParens = bool OnlyOpenParen BothParens

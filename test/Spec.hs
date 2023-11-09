{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck.All
import GlobRegex

prop_globmatcher1 cs =
  case globMatcher "[!a-zA-Z0-9]" cs of
    (Right b1) ->
      case globMatcher "[a-zA-Z0-9]" cs of
        (Right b2) ->
          (and (or b1 b2) (not (and b1 b2)))
prop_globmatcher2 cs =
  case globMatcher "*" cs of
    (Right b) -> b
prop_globmatcher3 (c:[]) =
  case globMatcher "?" [c] of
    (Right b) -> b
prop_globmatcher3 (c:cs) =
  case globMatcher "?" [c] of
    (Right b) -> (and b (prop_globmatcher3 cs))

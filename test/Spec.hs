{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import GlobRegex

prop_globmatcher1 :: Char -> Bool
prop_globmatcher1 c =
  case globMatcher "[!a-zA-Z0-9]" [c] of
    (Right b1) ->
      case globMatcher "[a-zA-Z0-9]" [c] of
        (Right b2) ->
          (b1 || b2) && (not (b1 && b2))

prop_globmatcher2 :: [Char] -> Bool
prop_globmatcher2 cs =
  case globMatcher "*" cs of
    (Right b) -> b

help_globmatcher3 :: [Char] -> Bool
help_globmatcher3 (cs) =
  case globMatcher "?" cs of
    (Right b) -> b
prop_globmatcher3 :: [Char] -> Bool
prop_globmatcher3 ([c]) = help_globmatcher3 [c]
prop_globmatcher3 cs = not $ help_globmatcher3 cs

return []
runTests = $quickCheckAll

main :: IO Bool
main = runTests

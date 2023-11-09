-- | PathElem.hs

module PathElem (makeMatcherList) where

import System.FilePath (dropTrailingPathSeparator)
import GlobRegex (globMatcher)

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

makeMatcherList :: [String] -> [String -> Either String Bool]
makeMatcherList pl = map maybeCompile pl
  where maybeCompile e = let d = dropTrailingPathSeparator e
                         in (if isPattern d
                             then globMatcher d
                             else exactMatcher d)
                            .
                            dropTrailingPathSeparator
        exactMatcher pat str = Right $ pat == str

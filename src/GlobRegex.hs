-- | GlobRegex.hs

module GlobRegex (globMatcher) where

import Text.RE.TDFA.String (compileRegex,(=~))

whenRight :: (a -> b) -> (Either c a) -> (Either c b)
whenRight _ (Left e) = Left e
whenRight f (Right v) = Right (f v)

globToRegex :: String -> Either String String
globToRegex str = whenRight (\s -> '^' : s ++ "$") (globToRegex' str)
  where globToRegex' "" = Right ""
        globToRegex' ('*':cs) = whenRight (".*"++) (globToRegex' cs)
        globToRegex' ('?':cs) = whenRight ('.':) (globToRegex' cs)
        globToRegex' ('[':'!':c:cs) = whenRight (("[^"++) . (c:)) (charClass cs)
        globToRegex' ('[':c:cs)     = whenRight (('[':) . (c:)) (charClass cs)
        globToRegex' ('[':_)        = Left "unterminated character class"
        globToRegex' (c:cs) = whenRight (escape c++) (globToRegex' cs)

        escape c | c `elem` regexChars = '\\' : [c]
                 | otherwise = [c]
          where regexChars = "\\+()^$.{}]|"

        charClass (']':cs) = whenRight (']':) (globToRegex' cs)
        charClass (c:cs)   = whenRight (c:) (charClass cs)
        charClass []       = Left "unterminated character class"

globMatcher :: String -> String -> Either String Bool
globMatcher pat str = case whenRight compileRegex (globToRegex pat) of
                        (Right (Just re)) -> Right $ str =~ re
                        (Left s) -> Left s
                        _ -> Left "evalme_CPL_01"

-- | Lib.hs

module Lib (globMatching,PatternException) where

import PathElem (makeMatcherList)
import System.FilePath ((</>),isPathSeparator,splitPath,isRelative)
import System.Directory (doesDirectoryExist,getDirectoryContents)
import Control.Exception (throw,Exception)
import Control.Monad (filterM)

-- I'm not proficient at handling exceptions so that this section is still experimental.
newtype PatternException = PatternException String
  deriving (Show)
instance Exception PatternException

match :: (String -> Either String Bool) -> String -> Bool
match mat str =
  case mat str of
    (Left e) -> throw $ PatternException e
    (Right v) -> v

listMatches :: FilePath -> (String -> Either String Bool) -> IO [String]
listMatches dirName mat = do
    names <- getDirectoryContents dirName
    return (map (dirName</>) (filter (match mat) names))

globMatching :: String -> IO [String]
globMatching pat = let pat' = if isRelative pat
                              then "." </> pat
                              else pat
                       -- The prefix is always determined and removed from the list.
                       -- Then it is used as the original directory path.
                       (dr:ps) = splitPath pat'
                       ms = makeMatcherList ps
                   in allMatches ps ms dr
  where allMatches (p:[]) (m:[]) bs = do
          matches <- listMatches bs m
          if isDirectory p
          then filterM doesDirectoryExist matches
          else return matches
        allMatches (_:ps) (m:ms) bs = do
          matches <- listMatches bs m
          matches' <- filterM doesDirectoryExist matches
          results <- mapM (\dir -> allMatches ps ms dir) matches'
          return (concat results)
        allMatches [] [] bs = return [bs]

        isDirectory = isPathSeparator . last

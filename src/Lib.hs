-- | Lib.hs

module Lib (globMatching,PatternException) where

import PathElem (makeMatcherList)
import System.FilePath ((</>),isPathSeparator,splitPath)
import System.Directory (doesDirectoryExist,getCurrentDirectory,getDirectoryContents)
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
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    names <- getDirectoryContents dirName'
    return (filter (match mat) names)

globMatching :: String -> IO [String]
globMatching pat = let ps = splitPath pat
                   in  let ms = makeMatcherList ps
                       in allMatches ps ms ""
  where allMatches (p:[]) (m:[]) bs = do
          matches <- listMatches bs m
          matches' <- if isDirectory p
                      then filterM doesDirectoryExist matches
                      else return matches
          return (map (bs</>) matches')
        allMatches (_:ps) (m:ms) bs = do
          matches <- listMatches bs m
          matches' <- filterM doesDirectoryExist matches
          results <- mapM (\dir -> allMatches ps ms (bs</> dir)) matches'
          return (concat results)

        isDirectory = isPathSeparator . last

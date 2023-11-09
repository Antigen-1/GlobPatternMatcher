module Main (main) where

import Lib (globMatching)
import System.Environment (getProgName,getArgs)
import System.IO (hPutStrLn,stderr)

-- | A very tiny commandline parser
main :: IO ()
main = do
  arg <- getArgs
  case arg of
    (pat:[]) -> do
      list <- (globMatching pat)
      mapM_ putStrLn list
    _ -> do
      name <- getProgName
      hPutStrLn stderr ("Usage: " ++ name ++ " <pattern>")

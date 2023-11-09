module Main (main) where

import Lib (globMatching)
import System.Environment (getArgs)
import System.Environment (getProgName)

main :: IO ()
main = do
  arg <- getArgs
  case arg of
    (pat:[]) -> globMatching pat
    _ -> (error "Usage: " ++ getProgName ++ " [pattern]")


module Top (main) where

import Load (load)
import Solve (solve,summarize,answerFromTree)
import System.Environment (getArgs)
import Tests (runAll)

main :: IO ()
main = do
  getArgs >>= \case
    ["reg"] -> runAll False
    ["tests"] -> runAll True
    [x] -> run1 x
    args -> error (show ("args",args))

run1 :: FilePath -> IO ()
run1 file = do
  spec <- load file
  tree <- solve spec
  print (summarize tree)
  print (answerFromTree tree)

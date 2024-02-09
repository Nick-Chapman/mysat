
module Top (main) where

import Load (load)
import Spec (sizeInfo)
import Solve (solve,summarize,firstAnswer,printST)
import System.Environment (getArgs)
import Tests (runAll)
import Text.Printf (printf)

main :: IO ()
main = do
  getArgs >>= \case
    ["reg"] -> runAll False
    ["tests"] -> runAll True
    [x] -> run1 x
    args -> error (show ("args",args))

run1 :: FilePath -> IO ()
run1 file = do
  let _ = (firstAnswer,printST)
  spec <- load file
  printf "Spec:%s\n" (sizeInfo spec)
  tree <- solve spec
  let _full = False
  printST _full tree
  print (summarize tree)
  --answer <- firstAnswer tree
  --print answer
  pure ()

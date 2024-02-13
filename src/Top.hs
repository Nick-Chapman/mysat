
module Top (main) where

import Load (load)
import Spec (sizeInfo)
import Solve (solve,summarize,firstAnswer,printST)
import System.Environment (getArgs)
import Tests (runAll)
import Text.Printf (printf)
import qualified Bedlam (gen,pp)

main :: IO ()
main = do
  getArgs >>= \case
    ["bedlam","gen",file] -> Bedlam.gen file
    ["bedlam","pp"] -> getContents >>= Bedlam.pp
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
  pure ()

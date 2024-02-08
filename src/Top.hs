
module Top (main) where

import Load (load)
import Solve (solve)
import System.Environment (getArgs)
import Tests (runAll)

main :: IO ()
main = do
  getArgs >>= \case
    [x] -> run1 x
    [] -> runAll
    args -> error (show ("args",args))

run1 :: FilePath -> IO ()
run1 file = do
  spec <- load file
  res <- solve spec
  print res

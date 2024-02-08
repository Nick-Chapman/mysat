
module Tests (runAll) where

import Check (checkSatisyingAssigment)
import Load (load)
import Solve (solve,Answer(..))
import Text.Printf (printf)

runAll :: IO ()
runAll = do
  let n = maximum [ length name | (name,_) <- examples ]
  let justify s = s ++ replicate (n - length s) ' '
  bools <- sequence [ test1 justify expect name | (name,expect) <- examples ]
  let numTests = length bools
  let numPass = length [ () | res <- bools, res ]
  let numFail = numTests - numPass
  printf "%d tests ran; %s\n"
    numTests
    (if numFail > 0 then show numFail ++ " fail." else "all pass.")

test1 :: (String -> String) -> Expect -> FilePath -> IO Bool
test1 justify expect base = do
  let jbase = justify base
  let file = "cnf/"++base++".cnf"
  spec <- load file
  answer <- solve spec
  case (answer,expect) of
    (UnSat,UnSatisfiable) -> do
      printf "%s : PASS (UnSat)\n" jbase
      pure True
    (Sat ass,Satisfiable) -> do
      case checkSatisyingAssigment spec ass of
        True -> do
          printf "%s : PASS (Sat)\n" jbase
          pure True
        False -> do
          printf "%s : FAIL (Sat,checks wrong)\n" jbase
          pure False
    (UnSat,Satisfiable) -> do
      printf "%s : FAIL (answer:UnSat,expected:Sat)\n" jbase
      pure False
    (Sat{},UnSatisfiable) -> do
      printf "%s : FAIL (answer:Sat,expected:UnSat)\n" jbase
      pure False

data Expect = Satisfiable | UnSatisfiable deriving (Show)

examples :: [(FilePath,Expect)]
examples = [ (x,UnSatisfiable) | x <- us ] ++ [ (x,Satisfiable) | x <- ss ]
  where
    us =
      [ "running_example"
      ]
    ss =
      [ "queens"
      , "xor1"
      ]

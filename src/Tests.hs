
module Tests (runAll) where

import Check (checkSatisyingAssigment)
import Load (load)
import Solve (solve,SearchTree,summarize,answerFromTree)
import Spec (Spec,sizeInfo)
import System.Timeout (timeout)
import Text.Printf (printf)
import qualified Spec as A (Answer(..))

runAll :: Bool -> IO ()
runAll verbose = do
  let duration = 100000 -- 1/10 sec
  let n = maximum [ length name | (name,_) <- examples ]
  let justify s = s ++ replicate (n - length s) ' '
  xs <- sequence [ seeTest verbose duration justify expect name | (name,expect) <- examples ]
  let numTests = length xs
  let numTimeout = length [ () | Timeout{} <- xs ]
  let numPass = length [ () | Pass{} <- xs ]
  let numFail = length [ () | Fail <- xs ]
  printf "%d tests ran; %s\n"
    numTests
    (if numPass == numTests then "all pass." else
       printf "%s%s%d pass."
       (if numFail > 0 then printf "%d FAIL; " numFail else "")
       (if numTimeout > 0 then printf "%d timeout; " numTimeout else "")
       numPass)

data Expect = Sat | UnSat deriving Show

data Res = Pass SearchTree | Timeout | Fail

seeTest :: Bool -> Int -> (String -> String) -> Expect -> String -> IO Res
seeTest verbose duration justify expect base = do
  let file = base++".cnf"
  spec <- load file
  res <- runTest duration spec expect
  let z = sizeInfo spec
  let jbase = justify base
  let
    seeResult :: Res -> IO ()
    seeResult = \case
      Timeout -> do
        printf "%s : TIMEOUT (%s) (%s)\n" jbase (show expect) z
        pure ()
      Pass tree -> do
        if verbose
          then printf "%s : PASS (%s; %s) (%s)\n" jbase (show expect) (summarize tree) z
          else pure ()
      Fail -> do
        printf "%s : FAIL (%s) (%s)\n" jbase z (show expect) z
        pure ()

  seeResult res
  pure res

runTest :: Int -> Spec -> Expect -> IO Res
runTest duration spec expect = do
  timeout duration (solve spec) >>= \case
    Nothing -> pure Timeout
    Just tree -> do
      let answer = answerFromTree tree
      pure $
        case (answer,expect) of
          (A.UnSat,Sat) -> Fail
          (A.Sat{},UnSat) -> Fail
          (A.UnSat,UnSat) -> Pass tree
          (A.Sat ass,Sat) ->
            case checkSatisyingAssigment spec ass of
              True -> Pass tree
              False -> Fail

examples :: [(FilePath,Expect)]
examples = []
  ++ [ (x,UnSat) | x <- unsat ]
  ++ [ (x,Sat) | x <- sat ]
  where

    unsat =
      [ "cnf/running_example"
      , "k/cover/cover0045"
      , "k/cover/cover0019"
      , "k/cover/cover0038"
      , "k/cover/cover0040"
      , "k/cover/cover0009"
      , "k/cover/cover0044"
      , "k/cover/cover0017"
      , "k/cover/cover0010"
      , "k/cover/cover0030"
      , "k/cover/cover0007"
      , "k/cnf/diamond2"
      , "k/cnf/prime4294967297"
      , "k/cnf/add16"
      , "k/cnf/diamond3"
      , "k/cnf/add8"
      , "k/cnf/unit1"
      , "k/cnf/strash2"
      , "k/cnf/false"
      , "k/cnf/full2"
      , "k/cnf/def1"
      , "k/cnf/miter1"
      , "k/cnf/unit2"
      , "k/cnf/strash1"
      , "k/cnf/add4"
      , "k/cnf/full3"
      , "k/cnf/ph5"
      , "k/cnf/ph3"
      , "k/cnf/twocores3"
      , "k/cnf/add32"
      , "k/cnf/twocores1"
      , "k/cnf/ph2"
      , "k/cnf/unit3"
      , "k/cnf/ph6"
      , "k/cnf/eq2"
      , "k/cnf/add64"
      , "k/cnf/add128"
      , "k/cnf/full4"
      , "k/cnf/unit4"
      , "k/cnf/prime65537"
      , "k/cnf/unit6"
      , "k/cnf/ph4"
      , "k/cnf/diamond1"
      , "k/cnf/unit5"
      , "k/cnf/twocores2"
      ]

    sat =
      [ "cnf/queens"
      , "cnf/xor1"
      , "k/cover/cover0027"
      , "k/cover/cover0000"
      , "k/cover/cover0031"
      , "k/cover/cover0001"
      , "k/cover/cover0011"
      , "k/cover/cover0028"
      , "k/cover/cover0037"
      , "k/cover/cover0032"
      , "k/cover/cover0005"
      , "k/cover/cover0036"
      , "k/cover/cover0006"
      , "k/cover/cover0034"
      , "k/cover/cover0025"
      , "k/cover/cover0033"
      , "k/cover/cover0035"
      , "k/cover/cover0002"
      , "k/cover/cover0024"
      , "k/cover/cover0018"
      , "k/cover/cover0004"
      , "k/cover/cover0021"
      , "k/cover/cover0042"
      , "k/cover/cover0012"
      , "k/cover/cover0020"
      , "k/cover/cover0039"
      , "k/cover/cover0043"
      , "k/cover/cover0013"
      , "k/cover/cover0015"
      , "k/cover/cover0003"
      , "k/cover/cover0023"
      , "k/cover/cover0014"
      , "k/cover/cover0029"
      , "k/cover/cover0022"
      , "k/cover/cover0026"
      , "k/cover/cover0016"
      , "k/cover/cover0041"
      , "k/cnf/sqrt5041"
      , "k/cnf/xor1"
      , "k/cnf/prime529"
      , "k/cnf/bin1"
      , "k/cnf/prime121"
      , "k/cnf/prime1369"
      , "k/cnf/sqrt63001"
      , "k/cnf/sqrt11449"
      , "k/cnf/xor3"
      , "k/cnf/xor2"
      , "k/cnf/sqrt16129"
      , "k/cnf/eq3"
      , "k/cnf/sqrt259081"
      , "k/cnf/sqrt10609"
      , "k/cnf/prime49"
      , "k/cnf/xor4"
      , "k/cnf/sqrt10201"
      , "k/cnf/prime841"
      , "k/cnf/bin2"
      , "k/cnf/sqrt4489"
      , "k/cnf/sqrt6241"
      , "k/cnf/sqrt1042441"
      , "k/cnf/prime4"
      , "k/cnf/sqrt5329"
      , "k/cnf/sqrt11881"
      , "k/cnf/prime1849"
      , "k/cnf/and1"
      , "k/cnf/sqrt6889"
      , "k/cnf/prime961"
      , "k/cnf/sqrt2809"
      , "k/cnf/prime9"
      , "k/cnf/sqrt9409"
      , "k/cnf/bin3"
      , "k/cnf/sqrt3481"
      , "k/cnf/sqrt12769"
      , "k/cnf/eq1"
      , "k/cnf/ite1"
      , "k/cnf/prime169"
      , "k/cnf/sqrt7921"
      , "k/cnf/probe1"
      , "k/cnf/strash3"
      , "k/cnf/prime289"
      , "k/cnf/prime361"
      , "k/cnf/sqrt3721"
      , "k/cnf/prime2209"
      , "k/cnf/and2"
      , "k/cnf/tieshirt"
      , "k/cnf/prime25"
      , "k/cnf/prime1681"
      , "k/cnf/true"
      ]

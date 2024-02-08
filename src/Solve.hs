
module Solve
  ( solve, SearchTree, summarize
  , answerFromTree, Answer(..)
  ) where

import Data.List (intercalate)
import Spec (Spec(..),Clause(..),Literal(..),Answer(..))
import Text.Printf (printf)

-- TODO: keep track of the number of decisions we make,and how any conflicts we run into
data SearchTree = SearchTree
  { answer :: Answer
  , numSteps :: Int
  }

summarize :: SearchTree -> String
summarize SearchTree{numSteps} = printf "#steps:%d" numSteps

answerFromTree :: SearchTree -> Answer
answerFromTree SearchTree{answer} = answer

solve :: Spec -> IO SearchTree
solve Spec{clauses=clauses0} = top
  where
    doUnitProp = True

    top :: IO SearchTree
    top = do
      (numSteps,answer) <- go 0 (initState clauses0)
      pure SearchTree { answer, numSteps }

    maybeUnitProp = if doUnitProp then tryUnitProp else Just

    go :: Int -> State -> IO (Int,Answer)
    go n0 s0 = do
      let n = 1 + n0
      --print (n,s0)
      case maybeUnitProp s0 of
        Nothing -> do
          pure (n,UnSat) -- TODO: dont use answer for these partial results
        Just s -> do
          case areWeDone s of
            Just a -> do
              pure (n, a)
            Nothing -> do
              let x = pickLit s
              go n (extendAssNeverInvalid x s) >>= \case
                a@(_,Sat{}) -> do
                  pure a
                (n,UnSat) -> do
                  go n (extendAssNeverInvalid (inverse x) s)


-- because we made the decision only for a var not already decided
extendAssNeverInvalid :: Literal -> State -> State
extendAssNeverInvalid x s =
  case extendAss x s of
    Nothing -> error "invalid extension"
    Just s -> s


tryUnitProp :: State -> Maybe State
tryUnitProp s = do
  case unitClauseLits s of
    [] -> Just s
    xs -> tryExtends s xs
  where
    tryExtends :: State -> [Literal] -> Maybe State
    tryExtends s = \case
      [] -> tryUnitProp s
      x:xs -> do
        case extendAss x s of
          Nothing -> Nothing
          Just s' -> tryExtends s' xs

unitClauseLits :: State -> [Literal]
unitClauseLits State{toBeSat} = [ x | Clause [x] <- toBeSat ]


inverse :: Literal -> Literal
inverse = \case Pos x -> Neg x; Neg x -> Pos x

data State = State { toBeSat :: [Clause], ass :: [Literal] }

instance Show State where
  show State{toBeSat,ass} =
    "#clauses: " ++ show (length toBeSat) ++ ", ass: " ++ show ass
    --"ass: " ++ show ass ++ ", clauses: " ++ intercalate "|" (map show toBeSat)
    where _ = intercalate

initState :: [Clause] -> State
initState toBeSat = State {toBeSat, ass = []}

areWeDone :: State -> Maybe Answer
areWeDone State{toBeSat,ass} =
  case toBeSat of
    [] -> Just (Sat ass)
    _ -> if any (\case Clause [] -> True; _ -> False) toBeSat then Just UnSat else Nothing

pickLit :: State -> Literal
pickLit State{toBeSat} =
  head [ lit | Clause lits <- toBeSat, lit <- lits ]


extendAss :: Literal -> State -> Maybe State
extendAss x State{toBeSat=clauses0,ass=ass0} =
  if inverse x `elem` ass0 then Nothing else
    Just $ State { toBeSat = map cantBeSat (filter (not . nowSat) clauses0)
                 , ass = x : ass0
                 }
      where
        nowSat (Clause xs) = x `elem` xs
        cantBeSat (Clause xs) = Clause (filter (/= inverse x) xs)

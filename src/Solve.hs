
module Solve
  ( solve
  , Answer(..)
  ) where

import Spec (Spec(..),Clause(..),Literal(..))

data Answer = UnSat | Sat [Literal]

instance Show Answer where
  show = \case
    UnSat -> "UNSAT"
    Sat xs -> "v " ++ show (Clause xs)

solve :: Spec -> IO Answer
solve Spec{clauses=clauses0} = go (initState clauses0)
  where
    go :: State -> IO Answer
    go s = do
      case areWeDone s of
        Just a -> do
          pure a
        Nothing -> do
          let x = pickLit s
          go (extendAss x s) >>= \case
            a@Sat{} -> do
              pure a
            UnSat -> do
              go (extendAss (inverse x) s)

inverse :: Literal -> Literal
inverse = \case Pos x -> Neg x; Neg x -> Pos x

data State = State { toBeSat :: [Clause], ass :: [Literal] }

instance Show State where
  show State{toBeSat,ass} =
    "#clauses: " ++ show (length toBeSat) ++ ", ass: " ++ show ass

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

extendAss :: Literal -> State -> State
extendAss x State{toBeSat=clauses0,ass=ass0} =
  if inverse x `elem` ass0 then error "invalid extension" else
    State { toBeSat = map cantBeSat (filter (not . nowSat) clauses0)
          , ass = x : ass0
          }
      where
        nowSat (Clause xs) = x `elem` xs
        cantBeSat (Clause xs) = Clause (filter (/= inverse x) xs)

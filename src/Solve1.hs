
module Solve1 (solve,SearchTree,summarize,answerFromTree) where

import Data.List (intercalate)
import Spec (Spec(..),Clause(..),Literal(..),Answer(..))

-- TODO: keep track of the number of decisions we make,and how any conflicts we run into
data SearchTree = SearchTree
  { answer :: Answer
  , counts :: Counts
  }

summarize :: SearchTree -> String
summarize SearchTree{counts} = show counts

answerFromTree :: SearchTree -> Answer
answerFromTree SearchTree{answer} = answer

solve :: Spec -> IO SearchTree
solve Spec{clauses=clauses0} = top
  where

    top :: IO SearchTree
    top = do
      (counts,answer) <- go counts0 (initState clauses0)
      pure SearchTree { answer, counts }

    go :: Counts -> State -> IO (Counts,Answer)
    go counts s0 = do
      case unitProp counts s0 of
        (counts,Nothing) -> pure (tickC counts,UnSat)
        (counts,Just s) -> do
          case areWeDone s of
            Just a -> pure (tickC counts, a)
            Nothing -> do
              let x = pickLit s
              go (tickD counts) (extendAssNeverInvalid x s) >>= \case
                a@(_,Sat{}) -> pure a
                (counts,UnSat) -> do
                  go (tickF counts) (extendAssNeverInvalid (inverse x) s)

data State = State { toBeSat :: [Clause], ass :: [Literal] }

instance Show State where
  show State{toBeSat,ass} =
    "#clauses: " ++ show (length toBeSat) ++ ", ass: " ++ show ass
    --"ass: " ++ show ass ++ ", clauses: " ++ intercalate "|" (map show toBeSat)
    where _ = intercalate

initState :: [Clause] -> State
initState toBeSat = State {toBeSat, ass = []}

-- because we made the decision only for a var not already decided
extendAssNeverInvalid :: Literal -> State -> State
extendAssNeverInvalid x s =
  case extendAss x s of
    Nothing -> error "invalid extension"
    Just s -> s

unitProp :: Counts -> State -> (Counts,Maybe State)
unitProp counts s = do
  case unitClauseLits s of
    [] -> (counts, Just s)
    xs -> tryExtends counts s xs
  where
    tryExtends :: Counts -> State -> [Literal] -> (Counts, Maybe State)
    tryExtends counts s = \case
      [] -> unitProp counts s
      x:xs -> do
        let counts' = tickF counts
        case extendAss x s of
          Nothing -> (counts', Nothing)
          Just s' -> tryExtends counts' s' xs

unitClauseLits :: State -> [Literal]
unitClauseLits State{toBeSat} = [ x | Clause [x] <- toBeSat ]

inverse :: Literal -> Literal
inverse = \case Pos x -> Neg x; Neg x -> Pos x

areWeDone :: State -> Maybe Answer
areWeDone State{toBeSat,ass} =
  case toBeSat of
    [] -> Just (Sat ass)
    _ -> if any (\case Clause [] -> True; _ -> False) toBeSat then Just UnSat else Nothing

pickLit :: State -> Literal
pickLit State{toBeSat} = head [ lit | Clause lits <- toBeSat, lit <- lits ]

extendAss :: Literal -> State -> Maybe State
extendAss x State{toBeSat=clauses0,ass=ass0} =
  if inverse x `elem` ass0 then Nothing else
    Just $ State { toBeSat = map cantBeSat (filter (not . nowSat) clauses0)
                 , ass = x : ass0
                 }
      where
        nowSat (Clause xs) = x `elem` xs
        cantBeSat (Clause xs) = Clause (filter (/= inverse x) xs)

data Counts = Counts
  { decisions :: Int
  , forced :: Int
  , conflicts :: Int
  } deriving Show

counts0 :: Counts
counts0 = Counts
  { decisions = 0
  , forced = 0
  , conflicts = 0
  }

tickD :: Counts -> Counts
tickD c@Counts{decisions} = c { decisions = 1 + decisions }

tickF :: Counts -> Counts
tickF c@Counts{forced} = c { forced = 1 + forced }

tickC :: Counts -> Counts
tickC c@Counts{conflicts} = c { conflicts = 1 + conflicts }

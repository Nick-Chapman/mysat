
module Solve2 (solve,SearchTree,printST,summarize,firstAnswer) where

import Spec (Spec(..),Clause(..),Literal(..),Answer)
import qualified Spec as A
import Text.Printf (printf)

data SearchTree
  = ST_Sat Level [Literal] (IO SearchTree)
  | ST_Conflict Level SearchTree
  | ST_Left Level Literal SearchTree
  | ST_Right Level Literal SearchTree
  | ST_Done

type Level = Int

summarize :: SearchTree -> String
summarize = show . trav 0 0
  where
    trav :: Int -> Int -> SearchTree -> Counts
    trav d c = \case
      ST_Left _ _ tree -> trav (d+1) c tree
      ST_Conflict _ tree -> trav d (c+1) tree
      ST_Right _ _ tree -> trav d c tree
      ST_Sat{} -> stop
      ST_Done -> stop
      where
        stop = Counts { decisions = d, conflicts = c }

data Counts = Counts
  { decisions :: Int
  , conflicts :: Int
  } deriving Show

firstAnswer :: SearchTree -> IO Answer
firstAnswer = pure . look
  where
    look :: SearchTree -> Answer
    look = \case
      ST_Done{} -> A.UnSat
      ST_Sat _ ass _more -> A.Sat ass
      ST_Conflict _ k -> look k
      ST_Left _ _ tree -> look tree
      ST_Right _ _ tree -> look tree

printST :: Bool -> SearchTree -> IO ()
printST full  = trav
  where
    tab n = replicate (2*n) ' '

    trav :: SearchTree -> IO ()
    trav = \case
      ST_Sat n xs k -> do
        printf "%sSat[%s]\n" (tab n) (show (Clause xs))
        if full then do tree <- k; trav tree else pure ()
      ST_Conflict _n tree -> do
        --printf "%sConflict\n" (tab _n)
        trav tree
      ST_Left n x left -> do
        printf "%sL[%s]\n" (tab n) (show x)
        trav left
      ST_Right n x tree -> do
        printf "%sR[%s]\n" (tab n) (show x)
        trav tree
      ST_Done{} -> do
        pure ()

solve :: Spec -> IO SearchTree
solve spec = go 0 (initState spec) (pure ST_Done)
  where
    go :: Level -> State -> IO SearchTree -> IO SearchTree
    go n s k = do
      case unitProp s of
        Nothing -> do
          tree <- k
          pure (ST_Conflict n tree)
        Just s -> do
          m <- areWeDone n k s
          case m of
            Just t -> pure t
            Nothing -> do
              let x = pickLit s
              let right = go (n+1) (extendAssNeverInvalid (inverse x) s) k
              let k_right = ST_Right n (inverse x) <$> right
              left <- go (n+1) (extendAssNeverInvalid x s) k_right
              pure (ST_Left n x left)

areWeDone :: Level -> IO SearchTree -> State -> IO (Maybe SearchTree) -- TODO: inline
areWeDone n k s =
  case areWeSatisfied s of
    Just ass -> pure (Just (ST_Sat n ass k))
    Nothing ->
      if areWeConflicted s
      then do
        tree <- k
        pure (Just (ST_Conflict n tree))
      else
        pure Nothing

extendAssNeverInvalid :: Literal -> State -> State
extendAssNeverInvalid x s =
  case extendAss x s of
    Nothing -> error "invalid extension"
    Just s -> s

inverse :: Literal -> Literal
inverse = \case Pos x -> Neg x; Neg x -> Pos x

data State = State { toBeSat :: [Clause], ass :: [Literal] }

initState :: Spec -> State
initState Spec{clauses} = State {toBeSat = clauses, ass = []}

unitProp :: State -> Maybe State
unitProp s = do
  case unitClauseLits s of
    [] -> Just s
    xs -> tryExtends s xs

tryExtends :: State -> [Literal] -> Maybe State
tryExtends s = \case
  [] -> unitProp s
  x:xs -> do
    case extendAss x s of
      Nothing -> Nothing
      Just s' -> tryExtends s' xs

unitClauseLits :: State -> [Literal]
unitClauseLits State{toBeSat} = [ x | Clause [x] <- toBeSat ]

areWeSatisfied :: State -> Maybe [Literal]
areWeSatisfied State{toBeSat,ass} =
  case toBeSat of
    [] -> Just ass
    _ -> Nothing

areWeConflicted :: State -> Bool
areWeConflicted State{toBeSat} =
    any (\case Clause [] -> True; _ -> False) toBeSat

extendAss :: Literal -> State -> Maybe State
extendAss x State{toBeSat=clauses0,ass=ass0} =
  if inverse x `elem` ass0 then Nothing else
    Just $ State { toBeSat = map cantBeSat (filter (not . nowSat) clauses0)
                 , ass = x : ass0
                 }
      where
        nowSat (Clause xs) = x `elem` xs
        cantBeSat (Clause xs) = Clause (filter (/= inverse x) xs)

pickLit :: State -> Literal
pickLit State{toBeSat} = head [ lit | Clause lits <- toBeSat, lit <- lits ]

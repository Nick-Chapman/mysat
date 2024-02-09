
module Solve2 (solve,SearchTree,printST,summarize,firstAnswer) where

import Data.List (maximumBy)
import Data.Map (Map)
import Data.Ord (comparing)
import Spec (Spec(..),Clause(..),Literal(..),Answer,Var)
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Spec as A

nub :: Ord a => [a] -> [a]
nub = Set.toList . Set.fromList

data SearchTree
  = ST_Sat Level [Literal] (IO SearchTree)
  | ST_Conflict Level State SearchTree
  | ST_Left Level Literal State SearchTree
  | ST_Right Level Literal State SearchTree
  | ST_Done

type Level = Int

summarize :: SearchTree -> String
summarize = show . trav 0 0
  where
    trav :: Int -> Int -> SearchTree -> Counts
    trav d c = \case
      ST_Left _ _ _ tree -> trav (d+1) c tree
      ST_Conflict _ _ tree -> trav d (c+1) tree
      ST_Right _ _ _ tree -> trav d c tree
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
      ST_Conflict _ _ k -> look k
      ST_Left _ _ _ tree -> look tree
      ST_Right _ _ _ tree -> look tree

printST :: Bool -> SearchTree -> IO ()
printST full  = trav
  where
    tab n = replicate (2*n) ' '

    trav :: SearchTree -> IO ()
    trav = \case
      ST_Sat n xs k -> do
        printf "%sSat[%s]\n" (tab n) (show (Clause xs))
        if full then do tree <- k; trav tree else pure ()
      ST_Conflict _n s tree -> do
        printf "%sConflict(%s)\n" (tab _n) (sizeState s)
        trav tree
      ST_Left n x s tree -> do
        printf "%sL[%s](%s)\n" (tab n) (show x) (sizeState s)
        trav tree
      ST_Right n x s tree -> do
        printf "%sR[%s](%s)\n" (tab n) (show x) (sizeState s)
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
          pure (ST_Conflict n s tree)
        Just s -> do
          m <- areWeDone n k s
          case m of
            Just t -> pure t
            Nothing -> do
              let x = pickLit s
              let sR = extendAssNeverInvalid (inverse x) s
              let right = go (n+1) sR k
              let k_right = ST_Right n (inverse x) sR <$> right
              let sL = extendAssNeverInvalid x s
              left <- go (n+1) sL k_right
              pure (ST_Left n x sL left)

areWeDone :: Level -> IO SearchTree -> State -> IO (Maybe SearchTree) -- TODO: inline
areWeDone n k s =
  case areWeSatisfied s of
    Just ass -> pure (Just (ST_Sat n ass k))
    Nothing ->
      if areWeConflicted s
      then do
        tree <- k
        pure (Just (ST_Conflict n s tree))
      else
        pure Nothing

extendAssNeverInvalid :: Literal -> State -> State
extendAssNeverInvalid x s =
  case extendAss x s of
    Nothing -> error "invalid extension"
    Just s -> s

inverse :: Literal -> Literal
inverse = \case Pos x -> Neg x; Neg x -> Pos x

data State = State { spec :: Spec, toBeSat :: [Clause], ass :: [Literal] }

sizeState :: State -> String
sizeState State{spec=Spec{nVars},toBeSat,ass} = printf "%d/%d" nv (length toBeSat)
  where
    nv = nVars - length ass

initState :: Spec -> State
initState spec@Spec{clauses} = State {spec, toBeSat = clauses, ass = []}

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
unitClauseLits State{toBeSat} = nub [ x | Clause [x] <- toBeSat ]

areWeSatisfied :: State -> Maybe [Literal]
areWeSatisfied State{toBeSat,ass} =
  case toBeSat of
    [] -> Just ass
    _ -> Nothing

areWeConflicted :: State -> Bool
areWeConflicted State{toBeSat} =
    any (\case Clause [] -> True; _ -> False) toBeSat

extendAss :: Literal -> State -> Maybe State
extendAss x state@State{toBeSat=clauses0,ass=ass0} =
  if inverse x `elem` ass0 then Nothing else
    Just $ state { toBeSat = map cantBeSat (filter (not . nowSat) clauses0)
                 , ass = x : ass0
                 }
      where
        nowSat (Clause xs) = x `elem` xs
        cantBeSat (Clause xs) = Clause (filter (/= inverse x) xs)

pickLit :: State -> Literal
pickLit = _pickB
  where
    _pickA State{toBeSat=clauses} = do -- dumb
      head [ lit | Clause lits <- clauses, lit <- lits ]

    _pickB s = do -- smart
      pickV (makeVocs s)

-- Var Occurance Counts by Sense(pos/neg)
data Vocs = Vocs (Map Var PN) deriving Show
type PN = (Int,Int)

sumPN :: PN -> PN -> PN
sumPN (p1,n1) (p2,n2) = (p1+p2,n1+n2)

makeVocs :: State -> Vocs
makeVocs State{toBeSat=clauses} = do
  Vocs $ Map.fromListWith sumPN $
    [ case lit of Pos x -> (x,(1,0)); Neg x -> (x,(0,1))
    | Clause lits <- clauses, lit <- lits
    ]

pickV :: Vocs -> Literal
pickV (Vocs m) = do
  let secondSum (_,(p,n)) = p+n
  let (v,(p,n)) = maximumBy (comparing secondSum) (Map.toList m)
  if p>n then Pos v else Neg v -- prefer neg literal when tied

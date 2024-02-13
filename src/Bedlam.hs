
module Bedlam (encodeCnf,decodeSolution,ppSolution) where

import qualified Spec
import Load (decodeAss)

import Data.List (intercalate,(\\))
import qualified Data.Map as Map
import Data.Map (Map)

encodeCnf :: Spec.Spec
encodeCnf = lowerSpec bedlam

decodeSolution :: String -> Solution
decodeSolution s = do
  let xs = [ x | Pos x <- map raiseLit (decodeAss s) ]
  let sol = recreateSolution xs
  sol

data Solution = Solution (Map Coord Piece)

recreateSolution :: [Var] -> Solution
recreateSolution xs = do
  let m = Map.fromList [ (c,p) | Var_PieceInCoord p c <- xs ]
  Solution m

ppSolution :: Solution -> String
ppSolution (Solution m) = do
  let look c = maybe '.' charOfPiece (Map.lookup c m)
  intercalate "\n"
    [ unlines [ [ look (x,y,z) | x <- allPos ]
              | y <- allPos
              ]
    | z <- allPos
    ]

----------------------------------------------------------------------
-- high level
data Spec = Spec [Clause]
data Clause = Clause [Literal]
data Literal = Pos Var | Neg Var

----------------------------------------------------------------------
-- lower

lowerSpec :: Spec -> Spec.Spec
lowerSpec (Spec clauses0) = Spec.Spec {nVars,nClauses,clauses}
  where
    clauses = map lowerClause clauses0
    nClauses = length clauses
    nVars = maximum [ lowerVar (varOfLit lit) | Clause lits <- clauses0, lit <- lits ]
    varOfLit = \case Pos x -> x; Neg x -> x

lowerClause :: Clause -> Spec.Clause
lowerClause (Clause lits) = Spec.Clause (map lowerLit lits)

lowerLit :: Literal -> Spec.Literal
lowerLit = \case
  Pos x -> Spec.Pos (lowerVar x)
  Neg x -> Spec.Neg (lowerVar x)

lowerVar :: Var -> Spec.Var
lowerVar v = maybe err id (Map.lookup v m)
  where m = Map.fromList (zip allVars [1..])
        err = error (show ("lowerV",v))

----------------------------------------------------------------------
-- raise

raiseLit :: Spec.Literal -> Literal
raiseLit = \case
  Spec.Pos x -> Pos (raiseVar x)
  Spec.Neg x -> Neg (raiseVar x)

raiseVar :: Spec.Var -> Var
raiseVar n = maybe err id (Map.lookup n m)
  where m = Map.fromList (zip [1..] allVars)
        err = error (show ("raiseV",n))

----------------------------------------------------------------------
-- bedlam vars

data Var
  = Var_PieceInCoord Piece Coord
  deriving (Eq,Ord,Show)

allVars :: [Var]
allVars = [ Var_PieceInCoord p c | p <- allPiece, c <- allCoord ]

----------------------------------------------------------------------
-- bedlam problem space

type Coord = (Pos,Pos,Pos)

data Pos = A | B | C | D deriving (Eq,Ord,Show)

allCoord :: [Coord]
allCoord = [ (x,y,z) | x <- allPos, y <- allPos, z <- allPos ]

allPos :: [Pos]
allPos = [A,B,C,D]

data Piece = Piece Char deriving (Eq,Ord,Show)
charOfPiece :: Piece -> Char
charOfPiece (Piece c) = c

allPiece :: [Piece]
allPiece = [ Piece c | c <- "abcd" ]

bedlam :: Spec
bedlam = spec
  where
    spec = Spec (pos++neg)
    pos = [ Clause [Pos x] | x <- xs ]
    neg = [ Clause [Neg x] | x <- allVars \\ xs ]
    xs =
      [ Var_PieceInCoord (Piece 'a') (A,A,A)
      , Var_PieceInCoord (Piece 'b') (B,B,B)
      ]

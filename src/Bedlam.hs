
module Bedlam (gen,pp) where

import qualified Spec
import Load (decodeAss)

import Data.List (intercalate)
import Data.List ((\\))
import qualified Data.Map as Map
import Data.Map (Map)
import Text.Printf (printf)

gen :: FilePath -> IO ()
gen fp = do
  mapM_ print [ (p, length xs) | (p,xs) <- Map.toList orientatedPiecePos ]
  printf "writing file: %s\n" fp
  writeFile fp (Spec.ppSpec (lowerSpec bedlam))

pp :: String -> IO ()
pp str = do
  let lits = map raiseLit (decodeAss str)
  putStr (ppSolution (recreateSolution [ x | Pos x <- lits ]))
  mapM_ print [ x | Pos (x@Var_PieceInOrientation{}) <- lits ]

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
data Literal = Pos Var | Neg Var
data Clause = Clause [Literal]
data Spec = Spec [Clause]

instance Show Literal where
  show = \case
    Pos x -> show x
    Neg x -> "-" ++ show x

instance Show Clause where
  show (Clause xs) = unwords (map show xs)

instance Show Spec where
  show (Spec xs) =
    unlines (map show xs)

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
-- pos

data Pos = A | B | C | D deriving (Eq,Ord,Show)

allPos :: [Pos]
allPos = [A,B,C,D]

invert :: Pos -> Pos
invert = \case A -> D; B -> C; C -> B; D -> A

shift :: Pos -> Pos
shift = \case
  A -> B
  B -> C
  C -> D
  D -> error "shiftD"

shifters :: Pos -> [Pos -> Pos]
shifters = \case
  A -> [id, shift, shift.shift, shift.shift.shift]
  B -> [id, shift, shift.shift]
  C -> [id, shift]
  D -> [id]

----------------------------------------------------------------------
-- coord

type Coord = (Pos,Pos,Pos)

allCoord :: [Coord]
allCoord = [ (x,y,z) | x <- allPos, y <- allPos, z <- allPos ]


orientators :: [Coord -> Coord]
orientators =
  [ f1.f2.f3.f4
  | f1 <- [id,quarterXY]
  , f2 <- [id,halfXY]
  , f3 <- [id,quarterXZ]
  , f4 <- [id,clock,anti]
  ]
  where
    quarterXY (x,y,z) = (y,invert x,z)
    halfXY    (x,y,z) = (invert x, invert y, z)
    quarterXZ (x,y,z) = (z,y,invert x)
    clock     (x,y,z) = (y,z,x)
    anti      (x,y,z) = (z,x,y)

{-shifts :: [Coord] -> [[Coord]]
shifts cs =
  take 1
  [ map (fx.fy.fz) cs
  | fx <- [ (\(x,y,z) -> (f x, y, z)) | f <- shifters (maximum [ x | (x,_,_) <- cs ])]
  , fy <- [ (\(x,y,z) -> (x, f y, z)) | f <- shifters (maximum [ y | (_,y,_) <- cs ])]
  , fz <- [ (\(x,y,z) -> (x, y, f z)) | f <- shifters (maximum [ z | (_,_,z) <- cs ])]
  ]-}

orientations :: [Coord] -> [[Coord]]
orientations cs =
  --take 1
  [ map (fo.fx.fy.fz) cs
  | fx <- --take 1 -- HACK
          [ (\(x,y,z) -> (f x, y, z)) | f <- shifters (maximum [ x | (x,_,_) <- cs ])]
  , fy <- [ (\(x,y,z) -> (x, f y, z)) | f <- shifters (maximum [ y | (_,y,_) <- cs ])]
  , fz <- [ (\(x,y,z) -> (x, y, f z)) | f <- shifters (maximum [ z | (_,_,z) <- cs ])]
  , fo <- orientators
  ]


----------------------------------------------------------------------
-- pieces

data Piece = Piece Char deriving (Eq,Ord,Show)
charOfPiece :: Piece -> Char
charOfPiece (Piece c) = c

piecePositions :: Map Piece [Coord]
piecePositions =
  Map.fromList
  [ (Piece c,coords)
  | (c,xs) <- raw
  , let coords = map ppp xs
  ]

  where
    ppp (x,y,z) = (p x, p y, p z)
    p :: Int -> Pos
    p = \case 0 -> A; 1 -> B; 2 -> C; 3 -> D; _ -> error "p"

    raw =
      -- red pieces (should be uppercase)
      [ ('A',[(0,0,0),(1,0,0),(2,0,0),(0,1,0),(0,0,1)]) -- base-girder
      , ('B',[(0,0,0),(1,0,0),(1,1,0),(2,1,0),(2,1,1)]) -- spiral-staircase
      , ('C',[(0,0,0),(1,0,0),(1,1,0),(2,1,0),(1,1,1)]) -- fork
      , ('D',[(1,0,0),(1,1,0),(1,2,0),(0,1,0),(2,1,0)]) -- flat-cross
      -- blue
      , ('e',[(0,0,0),(1,0,0),(2,0,0),(1,1,0),(1,1,1)]) -- modern-art
      , ('f',[(0,0,0),(1,0,0),(2,0,0),(1,1,0),(0,0,1)]) -- bent-f-piece
      , ('g',[(0,0,0),(1,0,0),(1,1,0),(1,1,1),(2,1,1)]) -- s-piece
      , ('h',[(1,0,0),(0,1,0),(1,1,0),(2,1,0),(0,2,0)]) -- flat-tree
      -- yellow
      , ('v',[(0,0,0),(1,0,0),(2,0,0),(2,0,1),(2,1,1)]) -- L-sign-post
      , ('w',[(0,0,0),(1,0,0),(2,0,0),(2,1,0),(0,0,1)]) -- hug
      , ('x',[(0,0,0),(1,0,0),(2,0,0),(1,1,0),(1,0,1)]) -- middle-girder
      , ('y',[(0,0,0),(1,0,0),(1,1,0),(2,1,0),(2,2,0)]) -- simple-stairs
--      , ('z',[(0,0,0),(1,0,0),(1,1,0),(1,1,1)]) -- small-one
      ]

allPiece :: [Piece]
allPiece = [ p | (p,_) <- Map.toList piecePositions ]

----------------------------------------------------------------------
-- orientations
data Orientation = Orientation Int deriving (Eq,Ord,Show)

orientatedPiecePos :: Map Piece [ (Orientation,[Coord]) ]
orientatedPiecePos = do
  Map.fromList
    [ (p, [ (Orientation o,ys) | (o,ys) <- zip [1..] (orientations xs)])
    | (p,xs) <- Map.toList piecePositions
    ]

transposeOrientation :: [(Orientation,[Coord])] -> [(Coord,[Orientation])]
transposeOrientation xs = do
  let c2os = collate [ (c,o) | (o,cs) <- xs, c <- cs ]
  let cs = [ c | (c,_) <- c2os ]
  c2os ++ [ (c,[]) | c <- allCoord \\ cs ]

collate :: Ord k => [(k,v)] -> [(k,[v])]
collate xs = Map.toList (Map.fromListWith (++) [ (k,[v]) | (k,v) <- xs ])

orientatedPiecePosT :: Map Piece [ (Coord,[Orientation]) ]
orientatedPiecePosT = do
  Map.fromList
    [ (p, transposeOrientation xs)
    | (p,xs) <- Map.toList orientatedPiecePos
    ]


----------------------------------------------------------------------
-- bedlam vars

data Var
  = Var_PieceInCoord Piece Coord
  | Var_PieceInOrientation Piece Orientation
  deriving (Eq,Ord,Show)

allVars :: [Var]
allVars =
  []
  ++ [ Var_PieceInCoord p c
     | p <- allPiece, c <- allCoord
     ]
  ++ [ Var_PieceInOrientation p o
     | (p,xs) <- Map.toList orientatedPiecePos
     , (o,_) <- xs
     ]

locatedOrientatedPieces :: [Clause]
locatedOrientatedPieces =
  [ Clause [ Neg (Var_PieceInOrientation p o)
           , Pos (Var_PieceInCoord p c)
           ]
  | (p,xs) <- Map.toList orientatedPiecePos
  , (o,cs) <- xs
  , c <- cs
  ]

_unlocatedOrientatedPieces :: [Clause]
_unlocatedOrientatedPieces =
  [ Clause ( Neg (Var_PieceInCoord p c)
             : [Pos (Var_PieceInOrientation p o) | o <- os ]
           )
  | (p,xs) <- Map.toList orientatedPiecePosT
  , (c,os) <- xs
  ]

notInSameSpot :: [Clause]
notInSameSpot =
  [ Clause [ Neg (Var_PieceInCoord p1 c)
           , Neg (Var_PieceInCoord p2 c)
           ]
  | p1 <- allPiece
  , p2 <- allPiece
  , p1 /= p2
  , c <- allCoord
  ]

pieceInSomeOrientation :: [Clause]
pieceInSomeOrientation =
  [ Clause [Pos (Var_PieceInOrientation p o) | (o,_) <- xs ]
  | (p,xs) <- Map.toList orientatedPiecePos
  ]

pieceMaxOneOrientation :: [Clause]
pieceMaxOneOrientation =
  [ Clause [ Neg (Var_PieceInOrientation p o1)
           , Neg (Var_PieceInOrientation p o2) ]
  | (p,xs) <- Map.toList orientatedPiecePos
  , (o1,_) <- xs
  , (o2,_) <- xs
  , o1 /= o2
  ]

bedlam :: Spec
bedlam = Spec $
  []
  ++ locatedOrientatedPieces
  ++ _unlocatedOrientatedPieces -- for partial puzzles
  ++ notInSameSpot
  ++ pieceInSomeOrientation
  ++ pieceMaxOneOrientation


module Wooden (gen,pp) where

import Data.List ((\\))
import Data.List (intercalate)
import Data.Map (Map)
import Load (decodeAss)
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Spec

gen :: FilePath -> IO ()
gen fp = do
  info
  let spec = lowerSpec bedlam
  printf "writing file: %s (%s)\n" fp (Spec.sizeInfo spec)
  writeFile fp (Spec.ppSpec spec)
  printf "writing file: %s (%s) - DONE\n" fp (Spec.sizeInfo spec)

info :: IO ()
info = do
  sequence_
    [ printf "%s : %d\n" (show piece) (length xs)
    | (piece,xs) <- Map.toList orientatedPiecePos
    ]

pp :: String -> IO ()
pp str = do
  let lits = map raiseLit (decodeAss str)
  --mapM_ print [ x | Pos (x@Var_PieceInOrientation{}) <- lits ]
  putStr (ppSolution (recreateSolution [ x | Pos x <- lits ]))

data Solution = Solution (Map Coord Piece)

recreateSolution :: [Var] -> Solution
recreateSolution xs = do
  let m = Map.fromList [ (c,p) | PieceAt p c <- xs ]
  Solution m

ppSolution :: Solution -> String
ppSolution (Solution m) = do
  let look c = maybe '?' charOfPiece (Map.lookup c m)
  intercalate "\n"
    [ unlines [ [ look (x,y,z) | x <- allPos ]
              | y <- allPos
              ]
    | z <- allPos
    ]

----------------------------------------------------------------------
-- bedlam constraints

locatedOrientatedPieces :: [Clause]
locatedOrientatedPieces =
  [ Clause [ Neg (PieceOrientation p o)
           , Pos (PieceAt p c)
           ]
  | (p,xs) <- Map.toList orientatedPiecePos
  , (o,cs) <- xs
  , c <- cs
  ]

unlocatedOrientatedPieces :: [Clause]
unlocatedOrientatedPieces =
  [ Clause ( Neg (PieceAt p c)
             : [Pos (PieceOrientation p o) | o <- os ]
           )
  | (p,xs) <- Map.toList orientatedPiecePosT
  , (c,os) <- xs
  ]

notInSameSpot :: [Clause]
notInSameSpot =
  [ Clause [ Neg (PieceAt p1 c)
           , Neg (PieceAt p2 c)
           ]
  | p1 <- allPiece
  , p2 <- allPiece
  , p1 /= p2
  , c <- allCoord
  ]

somethingEverywhere :: [Clause]
somethingEverywhere =
  [ Clause [ Pos (PieceAt p c)
           | p <- allPiece
           ]
  | c <- allCoord
  ]

pieceInSomeOrientation :: [Clause]
pieceInSomeOrientation =
  [ Clause [Pos (PieceOrientation p o) | (o,_) <- xs ]
  | (p,xs) <- Map.toList orientatedPiecePos
  ]

pieceMaxOneOrientation :: [Clause]
pieceMaxOneOrientation =
  [ Clause [ Neg (PieceOrientation p o1)
           , Neg (PieceOrientation p o2) ]
  | (p,xs) <- Map.toList orientatedPiecePos
  , (o1,_) <- xs
  , (o2,_) <- xs
  , o1 /= o2
  ]

bedlam :: Spec
bedlam = Spec $
  []
  ++ locatedOrientatedPieces
  ++ unlocatedOrientatedPieces
  ++ notInSameSpot
  ++ somethingEverywhere
  ++ pieceInSomeOrientation
  ++ pieceMaxOneOrientation

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
-- high level CNF spec

data Spec = Spec [Clause]
data Clause = Clause [Literal]
data Literal = Pos Var | Neg Var

data Var
  = PieceAt Piece Coord
  | PieceOrientation Piece Orientation
  deriving (Eq,Ord,Show)

allVars :: [Var]
allVars =
  []
  ++ [ PieceAt p c
     | p <- allPiece
     , c <- allCoord
     ]
  ++ [ PieceOrientation p o
     | (p,xs) <- Map.toList orientatedPiecePos
     , (o,_) <- xs
     ]

----------------------------------------------------------------------
-- position (on single axis of the cube)

data Pos = A | B | C | D | E deriving (Eq,Ord,Show)

allPos :: [Pos]
allPos = [A,B,C,D,E]

invert :: Pos -> Pos
invert = \case A -> E; B -> D; C -> C; D -> B; E -> A

shift :: Pos -> Pos
shift = \case
  A -> B
  B -> C
  C -> D
  D -> E
  E -> error "shiftE"

shifters :: Pos -> [Pos -> Pos]
shifters = \case
  A -> [id, shift, shift.shift, shift.shift.shift, shift.shift.shift.shift]
  B -> [id, shift, shift.shift, shift.shift.shift]
  C -> [id, shift, shift.shift]
  D -> [id, shift]
  E -> [id]

----------------------------------------------------------------------
-- 3d coordinate (of the cube)

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

orientations0 :: [Coord] -> [[Coord]]
orientations0 cs =
  [ map (fo.fx.fy.fz) cs
  | fx <- [ (\(x,y,z) -> (f x, y, z)) | f <- shifters (maximum [ x | (x,_,_) <- cs ])]
  , fy <- [ (\(x,y,z) -> (x, f y, z)) | f <- shifters (maximum [ y | (_,y,_) <- cs ])]
  , fz <- [ (\(x,y,z) -> (x, y, f z)) | f <- shifters (maximum [ z | (_,_,z) <- cs ])]
  , fo <- orientators
  ]

orientations :: [Coord] -> [[Coord]]
orientations =
  map Set.toList . nub . map Set.fromList . orientations0
  where
    nub = Set.toList . Set.fromList

----------------------------------------------------------------------
-- pieces

data Piece = Piece Char deriving (Eq,Ord,Show)
charOfPiece :: Piece -> Char
charOfPiece (Piece c) = c

piecePositions :: Map Piece [Coord]
piecePositions =
  Map.fromList [ (Piece c, map ppp xs) | (c,xs) <- raw ]
  where
    ppp (x,y,z) = (p x, p y, p z)
    p :: Int -> Pos
    p = \case 0 -> A; 1 -> B; 2 -> C; 3 -> D; 4 -> E; _ -> error "p"

    raw =
      [ (x,[(0,0,0),(1,0,0),(2,0,0),(2,1,0),(3,1,0)])
      | x <- ['a'..'y']
      ]

allPiece :: [Piece]
allPiece =
--  [ Piece '.' ] ++
  [ p | (p,_) <- Map.toList piecePositions ]

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

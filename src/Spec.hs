
module Spec
  ( Spec(..) , Literal(..) , Clause(..), Var
  , Answer(..)
  , sizeInfo
  ) where

import Text.Printf (printf)

data Spec = Spec
  { nVars :: Int
  , nClauses :: Int
  , clauses :: [Clause]
  }

sizeInfo :: Spec -> String
sizeInfo Spec{nVars,nClauses} = printf "%d/%d" nVars nClauses

data Clause = Clause [Literal]
data Literal = Pos Var | Neg Var deriving (Eq,Ord)
type Var = Int

instance Show Clause where
  show (Clause xs) = unwords (map show xs)

instance Show Literal where
  show = \case
    Pos x -> "+" ++ show x
    Neg x -> "-" ++ show x

data Answer = UnSat | Sat [Literal]

instance Show Answer where
  show = \case
    UnSat -> "UNSAT"
    Sat xs -> "v " ++ show (Clause xs)

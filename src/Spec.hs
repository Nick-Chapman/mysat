
module Spec
  ( Spec(..) , Literal(..) , Clause(..)
  , Answer(..)
  ) where

data Spec = Spec
  { nVars :: Int
  , nClauses :: Int
  , clauses :: [Clause]
  }

data Clause = Clause [Literal]
data Literal = Pos Var | Neg Var deriving Eq
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


module Check
  ( checkSatisyingAssigment
  ) where

import Spec (Spec(..),Clause(..),Literal(..))

checkSatisyingAssigment :: Spec -> [Literal] -> Bool
checkSatisyingAssigment Spec{clauses} ass = do
  all isSat clauses
    where
      isSat (Clause xs) =
        any (`elem` ass) xs

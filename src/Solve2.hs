
module Solve2 (solve,SearchTree,summarize,answerFromTree) where

import Spec --(Spec(..),Clause(..),Literal(..),Answer(..))

solve :: Spec -> IO SearchTree
solve = undefined

data SearchTree

summarize :: SearchTree -> String
summarize = undefined

answerFromTree :: SearchTree -> Answer
answerFromTree = undefined


module Load (load) where

import Par4 (Par,parse,terminated,nl,lit,key,int,ws0,ws1,alts,many,dot)
import Spec (Spec(..),Clause(..),Literal(..))

load :: String -> IO Spec
load x = parse gram <$> readFile x

gram :: Par Spec
gram = do
  _ <- many commentLine
  _ <- many nl
  lit 'p'; ws1; key "cnf"; ws1
  nVars <- int; ws1
  nClauses <- int; ws0; nl
  clauses <- terminated (do ws0; nl) clause
  pure Spec {nVars,nClauses,clauses}
  where
    commentLine = do lit 'c'; _ <- many dot; nl
    clause = do _ <- many nl; _ <- ws0; xs <- literals; pure (Clause xs)
    literals = do
      literal >>= \case
        Pos 0 -> pure []
        x -> do ws0; xs <- literals; pure (x:xs)
    literal = alts [ do lit '-'; x <- int; pure (Neg x)
                   , do x <- int; pure (Pos x) ]

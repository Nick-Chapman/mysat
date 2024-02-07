module Top (main) where

import System.Environment (getArgs)
import Par4 (Par,parse,terminated,nl,lit,key,int,ws0,ws1,alts)

main :: IO ()
main = do
  args <- getArgs
  let file = case args of [x] -> x; _ -> error (show ("args",args))
  let load x = parse gram <$> readFile x
  spec <- load file
  res <- solve spec
  print res

gram :: Par Spec
gram = do
  lit 'p'; ws1; key "cnf"; ws1
  nVars <- int; ws1
  nClauses <- int; ws0; nl
  clauses <- terminated (do ws0; nl) clause
  pure Spec {nVars,nClauses,clauses}
  where
    clause = do _ <- ws0; xs <- literals; pure (Clause xs)
    literals = do
      literal >>= \case
        Pos 0 -> pure []
        x -> do ws0; xs <- literals; pure (x:xs)
    literal = alts [ do lit '-'; x <- int; pure (Neg x)
                   , do x <- int; pure (Pos x) ]

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

solve :: Spec -> IO Answer
solve Spec{clauses=clauses0} = go (initState clauses0)
  where
    go :: State -> IO Answer
    go s = do
      case areWeDone s of
        Just a -> do
          pure a
        Nothing -> do
          let x = pickLit s
          go (extendAss x s) >>= \case
            a@Sat{} -> do
              pure a
            UnSat -> do
              go (extendAss (inverse x) s)


inverse :: Literal -> Literal
inverse = \case Pos x -> Neg x; Neg x -> Pos x

data State = State { toBeSat :: [Clause], ass :: [Literal] }

instance Show State where
  show State{toBeSat,ass} =
    "#clauses: " ++ show (length toBeSat) ++ ", ass: " ++ show ass

initState :: [Clause] -> State
initState toBeSat = State {toBeSat, ass = []}

areWeDone :: State -> Maybe Answer
areWeDone State{toBeSat,ass} =
  case toBeSat of
    [] -> Just (Sat ass)
    _ -> if any (\case Clause [] -> True; _ -> False) toBeSat then Just UnSat else Nothing

pickLit :: State -> Literal
pickLit State{toBeSat} =
  head [ lit | Clause lits <- toBeSat, lit <- lits ]

extendAss :: Literal -> State -> State
extendAss x State{toBeSat=clauses0,ass=ass0} =
  if inverse x `elem` ass0 then error "invalid extension" else
    State { toBeSat = map cantBeSat (filter (not . nowSat) clauses0)
          , ass = x : ass0
          }
      where
        nowSat (Clause xs) = x `elem` xs
        cantBeSat (Clause xs) = Clause (filter (/= inverse x) xs)

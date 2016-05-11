
module Super where

import Data.List
import Control.Arrow
import Debug.Trace

import Expr
import Pretty
--
-- supercompile :: Expr -> Expr
-- supercompile expr = envToLet hist (selExpr state)
--   where (hist, state) = reduce 0 [] (newstate expr)

newstate :: Expr -> State
newstate = (,,) [] []

showEnv :: [(Var, Expr)] -> String
showEnv env = intercalate "\n" (map (\(v,e)->v ++ " |-> " ++show e) env)

showList' :: Show a => [a] -> String
showList' xs = intercalate "\n" (map show xs)

showState :: State -> String
showState (env, stack, expr) =
  showEnv env ++ "\n" ++ show stack ++ "\n" ++ show expr

showRed :: (Hist, State) -> String
showRed (hist, (env, stack, expr)) =
  showEnv hist ++ "\n\n" ++
  showEnv env ++ "\n" ++ show stack ++ "\n" ++ show expr

-- | State of the eval machine.
type State = (Env, Stack, Expr)

-- | Environment that binds variables to values.
type Env = [(Var, Expr)]

-- | Stack for application calls.
type Stack = [StackFrame]

-- Stack frame for stack.
data StackFrame
  = Alts [(Pat, Expr)]
  | Arg Expr
  | Val Expr
  | Update Var
  deriving Show

put :: Var -> Expr -> Env -> Env
put var expr [] = [(var, expr)]
put var expr ((var',expr'):env) = if var' == var
  then (var,expr):env
  else (var',expr'):put var expr env

step :: State -> Maybe State
step (env, stack, expr) = --trace (show (env, stack, expr)) $
  case expr of
  Var var -> case lookup var env of
    Nothing -> Nothing
    Just val -> Just (env, Update var:stack, val)
  val@(Con tag args) -> case stack of
    [] -> Nothing
    -- Arg (Con tag' args'):stack' ->
    --   Just (env, stack', Con tag (args ++ [Con tag' args']))
    -- Arg argexpr:stack' ->
    --   Just (env, Val (Con tag args):stack', argexpr)
    -- Val (Con tag' args'):stack' ->
    --   Just (env, stack', Con tag' (args' ++ [Con tag args]))
    Alts alts:stack' ->
      let (Pat _ patvars, altexpr) = lookupAlt tag alts
      in Just (env, stack', substAlts (zip patvars args) altexpr)
    Update x:stack' -> Just (put x val env, stack', val)
    Arg argexpr:stack' ->
      Just (env, stack', Con tag (args ++ [argexpr]))
  val@(Lam var lamexpr) -> case stack of
    [] -> Nothing
    Arg argexpr:stack' -> Just (env, stack', subst (var, argexpr) lamexpr)
    Update x:stack' -> Just (put x val env, stack', val)
  Let var valexpr inexpr ->
    Just (put var valexpr env, stack, inexpr)
  App funexpr valexpr ->
    Just (env, Arg valexpr:stack, funexpr)
  Case scexpr alts -> Just (env, Alts alts:stack, scexpr)

-- | Reduce a state to Weak head normal form.
reduce :: State -> State
reduce state = case step state of
  Nothing -> state
  Just state' -> reduce state'

-- | Reduce a state to normal form.
-- | A normal form is either a constructor (Con) or
-- | lambda abstraction (Lam).
nf :: State -> State
nf state = case reduce state of
  (env, [], Con tag args) ->
    (env, [], Con tag (map (selExpr . nf . (,,) env []) args))
  (env, stack, Con _ _) -> error "Stack/Con"
  state' -> state'


type Hist = [(Var, Expr)]

match :: Expr -> Expr -> Bool
match expr expr' = expr == expr'

lookupMatch :: Hist -> Expr -> Maybe Var
lookupMatch [] _ = Nothing
lookupMatch ((var, expr'):hist) expr = if match expr expr'
  then Just var
  else lookupMatch hist expr

data HistM = H Hist Int

newhist :: HistM
newhist = H [] 0

bindH :: HistM -> (a -> HistM) -> HistM
bindH (H hist count) f = error ""

selH (H env _ ) = env

putH :: Expr -> HistM -> HistM
putH expr (H hist count) = H (("$h" ++ show count, expr):hist) (count+1)

tohist :: [Expr] -> HistM
tohist [] = newhist
tohist (e:es) = putH e (tohist es)

reduce' :: Int -> Hist -> State -> (Hist, State)
reduce' n hist state@(env,st,ex) = --trace (show state) $
  --case lookupMatch hist ex of
  --Nothing ->
    case step state of
    Nothing -> (hist, state)
    --case ex of--trace ("reduce: " ++ show ex ++ "@"++show st) ex of
      -- Var v t -> (hist, (env, st, foldl App (Var v t) st))
      -- Con tag args -> case reduces args n hist env of
      --   (args', h') ->
      ---- ((var n, Con tag args'):h', (env, st, Con tag args'))
      -- Case scexpr cases t ->
      --    case reduces (map snd cases) n hist env of
      --   (cs', h') -> (
      --          (var n, Case scexpr (zip (map fst cases) cs') t ):h',
      --        (env, st, Case scexpr (zip (map fst cases) cs') t ))
      -- _ -> error $ "Error with Split in: " ++ show ex
    --Just state' -> reduce (n+1) ((var n, selExpr state):hist) state'
    --Just state' -> reduce (n+1) ((var n, selExpr state):hist) state'
    Just state' -> reduce' (n+1) hist state'

  --Just var -> (hist, replState (Var var False) state)
  --Just var -> reduce n hist (replState (Var var False) state)
  where var n = "$v" ++ show n

reduces :: [Expr] -> Int -> Hist -> Env -> ([Expr], Hist)
reduces [] n hist env = ([], hist)
reduces (x:xs) n hist env = (selExpr s:xs', h')
  where (h, s) = reduce' (n+1) hist (env, [], x)
        (xs', h') = reduces xs (n+10) h env
        --var n = "#w_" ++ show n

envToLet :: Hist -> Expr -> Expr
envToLet [] expr = expr
envToLet ((var,valexpr):env) expr = Let var valexpr (envToLet env expr)

selExpr :: State -> Expr
selExpr state = let (_, _, expr) = state in expr

replState :: Expr -> State -> State
replState expr (env, stack, _) = (env, stack, expr)

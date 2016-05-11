
module Supercompile where

import Data.List
import Control.Arrow
import Debug.Trace

import Expr
import Eval
import Pretty

-- supercompile :: Expr -> Expr
-- supercompile expr = envToLet hist (selExpr state)
--   where (hist, state) = reduce 0 [] (newstate expr)

showEnv :: [(Var, Expr)] -> String
showEnv env = intercalate "\n" (map (\(v,e)->v ++ " |-> " ++show e) env)

showList' :: Show a => [a] -> String
showList' xs = intercalate "\n" (map show xs)

showState :: State -> String
showState (env, stack, expr) =
  showEnv env ++ "\n" ++ "show stack" ++ "\n" ++ show expr

showRed :: (Hist, State) -> String
showRed (hist, (env, stack, expr)) =
  showEnv hist ++ "\n\n" ++
  showEnv env ++ "\n" ++ "show stack" ++ "\n" ++ show expr



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





type Fresh = Int

aform' :: (Fresh, Env, Expr) -> (Fresh, Env, Expr)
aform' (fr, env, expr) = case expr of
  Var var -> (fr, env, expr)
  Lam var lamexpr -> case aform' (fr, env, lamexpr) of
    (fr', env', lamexpr') -> (fr', env', Lam var lamexpr')
  App funexpr valexpr -> case aform' (fr, env, valexpr) of
    (fr', env', valexpr') -> case aform' (fr', env', funexpr) of
      (fr'', env'', funexpr'') ->
        (fr''+2,
          (make (fr'' + 1), App funexpr'' (Var (make fr''))):
          (make fr'', valexpr'):env'',
          Var (make (fr''+1)) )
    --Let (make fr) (apply (aform (fr+1)) valexpr)
      --((makefr, valexpr) :env)
      --(App funexpr (usevar (make fr)))
    --expr -> apply (aform (fr+1)) expr
  where make i = "$v" ++ "_" ++ show i
      --  newfr = fr + 1

aform :: Expr -> Expr
aform expr = case aform' (0, [], expr) of
  (fr', env', expr') -> envToLet env' expr'

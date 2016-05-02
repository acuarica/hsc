
module Pretty where

import Expr

-- | Pretty prints an expression.
pprint' :: Bool -> Expr -> String
pprint' par expr = case expr of
  (Var var _) -> var
  (Con tag args) -> if null args
    then tag
    else paren (tag ++ " " ++ unwords (map pprint args))
  (Lam var expr) -> "(\\" ++ var ++ " -> " ++ pprint expr ++ ")"
  (Let var valexpr inexpr) -> "let " ++ var ++ "=" ++ pprint valexpr ++
    " in " ++ pprint inexpr
  (App aexpr vexpr) ->
     paren (pprint aexpr) ++ " " ++ paren (pprint vexpr)
  (Case sexpr cs) -> "case " ++ pprint sexpr ++ " of " ++
    foldr (\ (p, e) s -> pprint p ++ " -> " ++ pprint e ++ ";" ++ s) "" cs
  where paren s = if par then "(" ++ s ++ ")" else s

pprint :: Expr -> String
pprint = pprint' False

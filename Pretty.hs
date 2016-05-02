
module Pretty where

import Expr

-- | Pretty prints an expression.
-- | par indicates whether parenthesis are needed.
pretty' :: Bool -> Expr -> String
pretty' par expr = case expr of
  (Var var _) -> var
  (Con tag args) -> if null args
    then tag
    else paren (tag ++ " " ++ unwords (map pretty args))
  (Lam var expr) -> "(\\" ++ var ++ " -> " ++ pretty expr ++ ")"
  (Let var valexpr inexpr) -> "let " ++ var ++ "=" ++ pretty valexpr ++
    " in " ++ pretty inexpr
  (App aexpr vexpr) ->
     paren (pretty aexpr) ++ " " ++ paren (pretty vexpr)
  (Case sexpr cs) -> "case " ++ pretty sexpr ++ " of " ++
    foldr (\ (p, e) s -> pretty p ++ " -> " ++ pretty e ++ ";" ++ s) "" cs
  where paren s = if par then "(" ++ s ++ ")" else s

-- | Pretty prints an expression.
pretty :: Expr -> String
pretty = pretty' False

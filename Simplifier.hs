
module Simplifier (freduce, simp) where

import Control.Exception (assert)

import Expr (Expr(Var, Lam, Case), Var, Pat(Pat))
import Eval (Conf, StackFrame(Alts, Arg), reduce)

freduce :: Conf -> Conf
freduce = freduce' 1
  where
   v = Var . (++) "$m_" . show
   freduce' next conf = let (env, stack, expr) = reduce conf in
    case expr of
      Lam var lamexpr ->
        assert (null stack)
          (freduce' (next + 1) (env, [Arg (v next)], Lam var lamexpr))
      _ -> (env, stack, expr)

simp :: Conf -> Conf
simp conf@(env, stack, expr) = case expr of
  Var var -> case stack of
    Alts alts:Alts alts':stack' ->
      (env, stack', Case (Var var) (map (al alts') alts))
    _ -> conf
  _ -> conf
  where al als (Pat tag vars, altexpr) = (Pat tag vars, Case altexpr als)

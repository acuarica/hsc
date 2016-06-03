
module Simplifier where

import Control.Exception (assert)

import Expr
import Eval

doSimp :: Conf -> Conf
doSimp conf =
  let rconf'' = reduce conf in
  let rconf' = freduce (map (Var . (++) "$m_" . show) [1..10]) rconf'' in
  let rconf = reduce $ simp rconf' in
  rconf

freduce :: [Expr] -> Conf -> Conf
freduce args conf =
  let (env, stack, expr) = reduce conf in
  case expr of
    Lam var lamexpr ->
      assert (null stack)
        (freduce (tail args) (env, [Arg (head args)], Lam var lamexpr))
    _ -> (env, stack, expr)

simp :: Conf -> Conf
simp conf@(env, stack, expr) = case expr of
  Var var -> case stack of
    Alts alts:Alts alts':stack' ->
      (env, stack', Case (Var var) (map (al alts') alts))
    _ -> conf
  _ -> conf
  where al als (Pat tag vars, altexpr) = (Pat tag vars, Case altexpr als)

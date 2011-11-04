module Flite.Pretty where

import Flite.Syntax
import Data.List

consperse :: [a] -> [[a]] -> [a]
consperse x xs = concat (intersperse x xs)

pretty :: Prog -> String
pretty p = "{\n" ++ concatMap show p ++ "}"

instance Show Decl where
  show (Func name args rhs) = name ++ " "
                           ++ consperse " " (map showArg args)
                           ++ " = "
                           ++ show rhs ++ ";\n"
  show (Other str) = str ++ ";\n"

instance Show Exp where
  show (App e es) = consperse " " (showArg e : map showArg es)
  show (PrimApp p es) = "{" ++ show (App (Prim p) es) ++ "}"
  show (Case e as) = "case " ++ show e ++ " of " ++ showBlock showAlt as
  show (Let bs e) = "let " ++ showBlock showBind bs ++ " in " ++ show e
  show (Var v) = v
  show (Fun f) = f
  show (Prim f) = f
  show (Con c) = c
  show (Int i) = show i
  show (Alts as i) = "[" ++ consperse "," as ++ "]"
  show Bottom = "_|_"
  show (Ctr c arity i) = c
  show (Lam vs e) = '\\' : consperse " " vs ++ " -> " ++ show e
  show Wld = "_*"

showArg :: Exp -> String
showArg (App e []) = showArg e
showArg (App e es) = "(" ++ show (App e es) ++ ")"
showArg (Lam vs e) = "(" ++ show (Lam vs e) ++ ")"
showArg e = show e

showBlock :: (a -> String) -> [a] -> String
showBlock f as = "{ " ++ consperse "; " (map f as) ++ " }"

showAlt :: Alt -> String
showAlt (p, e) = show p ++ " -> " ++ show e

showBind :: Binding -> String
showBind (v, e) = v ++ " = " ++ show e

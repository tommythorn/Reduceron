module Flite.LambdaLift (lambdaLift) where

import Flite.Pretty --temp
import Flite.Syntax
import Flite.Traversals
import Flite.Descend
import Flite.WriterState
import Control.Monad

-- Introduces functions of the form "f^N" where is is a natural
-- number.  Therefore assumes function identifiers do not already
-- contain '^' character.

lambdaLift :: Char -> Prog -> Prog
lambdaLift c = concatMap (liftDecl c)

type Lift a = WriterState Decl Int a

liftDecl :: Char -> Decl -> [Decl]
liftDecl c (Func f args rhs) = Func f args rhs' : ds
  where
    (_, ds, rhs') = runWS (lift c f rhs) 0

lift :: Char -> Id -> Exp -> Lift Exp
lift c f (Lam [] e) = lift c f e
lift c f (Lam vs e) =
  do let ws = filter (`notElem` vs) (freeVars e)
     i <- get
     set (i+1)
     let f' = f ++ "^" ++ c : show i
     e' <- lift c f e
     write (Func f' (map Var (ws ++ vs)) e')
     return (App (Fun f') (map Var ws))
lift c f e = descendM (lift c f) e

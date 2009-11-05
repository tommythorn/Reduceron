module Flite.LambdaLift (lambdaLift) where

import Flite.Syntax
import Flite.Traversals
import Flite.Descend
import Flite.WriterState
import Control.Monad

-- Introduces functions of the form "f^N" where is is a natural
-- number.  Therefore assumes function identifiers do not already
-- contain '^' character.

lambdaLift :: Prog -> Prog
lambdaLift = concatMap liftDecl

type Lift a = WriterState Decl Int a

liftDecl :: Decl -> [Decl]
liftDecl (Func f args rhs) = Func f args rhs' : ds
  where
    (_, ds, rhs') = runWS (lift f rhs) 0

lift :: Id -> Exp -> Lift Exp
lift f (Lam [] e) = lift f e
lift f (Lam vs e) =
  do let ws = filter (`notElem` vs) (freeVars e)
     i <- get
     set (i+1)
     let f' = f ++ "^" ++ show i
     e' <- lift f e
     write (Func f' (map Var (ws ++ vs)) e')
     return (App (Fun f') (map Var ws))
lift f e = descendM (lift f) e

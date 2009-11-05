module Flite.Matching (desugarEqn, desugarCase) where

import Flite.Syntax
import Flite.Traversals
import Flite.Descend
import Flite.Fresh
import Data.List
import Data.Maybe
import Control.Monad

desugarEqn :: Prog -> Fresh Prog
desugarEqn p = mapM (\(f, arity, qs) -> 
                      do us <- mapM (\_ -> fresh) [1..arity]
                         rhs <- match us qs
                         return (Func f (map Var us) rhs)
                    ) (groupEqn p)

groupEqn :: Prog -> [(String, Int, [Equation])]
groupEqn p
  | all (rect . map funcArgs) dss = map gr dss
  | otherwise = error "Function equations cannot have different arities!"
  where
    dss = groupBy (\a b -> funcName a == funcName b) p

    gr ds = ( funcName (head ds)
            , length (funcArgs (head ds))
            , zip (map funcArgs ds) (map funcRhs ds)
            )

    rect :: [[a]] -> Bool
    rect = (== 1) . length . groupBy (==) . map length

desugarCase :: Prog -> Fresh Prog
desugarCase = onExpM (\e -> caseVar e >>= desugar)
  where
    desugar (Case (Var v) as) =
      do as' <- mapM (\(p, e) -> do e' <- desugar e; return (p, e')) as
         match [v] [([p], e) | (p, e) <- as']
    desugar e = descendM desugar e

caseVar :: Exp -> Fresh Exp
caseVar (Case e as) =
  case getVar e of
    Nothing -> do v <- fresh
                  caseVar (Let [(v, e)] (Case (Var v) as))
    Just v -> descendM caseVar (Case (Var v) as)
  where v = getVar e
caseVar e = descendM caseVar e

getVar :: Exp -> Maybe Id
getVar (Var v) = Just v
getVar (App e []) = getVar e
getVar e = Nothing

-- Wadler's algorithm for compilation of *uniform* pattern matching,
-- from "The Implementation of Functional Programming Languages".

type Equation = ([Pat], Exp)

isVar :: Equation -> Bool
isVar (Var v:ps, e) = True
isVar (App (Con c) args:ps, e) = False

isCon :: Equation -> Bool
isCon e = not (isVar e)

getCon :: Equation -> (Id, [Pat])
getCon (App (Con c) args:ps, e) = (c, args)

match :: [Id] -> [Equation] -> Fresh Exp
match [] [q] = return (snd q)
match (u:us) qs
  | all isVar qs = match us [(ps, subst (Var u) v e) | (Var v:ps, e) <- qs]
  | all isCon qs = do alts <- mapM (matchClause us) (groupEqns qs)
                      return (Case (Var u) alts)
match _ _ = error "Non-uniform pattern matching is disallowed!"

groupEqns :: [Equation] -> [(Id, Int, [Equation])]
groupEqns [] = []
groupEqns (q:qs)
  | all ((== arity) . length . snd . getCon) qs0 =
      (name, arity, qs0) : groupEqns qs1
  | otherwise = error ("Constructor `" ++ name ++ "` has different arities!")
  where (qs0, qs1) = partition ((== name) . fst . getCon) (q:qs)
        name = fst (getCon q)
        arity = length (snd (getCon q))

matchClause :: [Id] -> (Id, Int, [Equation]) -> Fresh Alt
matchClause us (c, arity, qs) =
  do us' <- mapM (\_ -> fresh) [1..arity]
     alts <- match (us' ++ us) [(ps' ++ ps, e) | (App (Con c) ps':ps, e) <- qs]
     return (App (Con c) (map Var us'), alts)

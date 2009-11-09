module Flite.Traversals where

import Flite.Syntax
import Flite.Descend
import Control.Monad
import Data.List
import Flite.Fresh

funcs :: Prog -> [String]
funcs p = [f | Func f args rhs <- p]

onExp :: (Exp -> Exp) -> Prog -> Prog
onExp f p = [Func g args (f rhs) | Func g args rhs <- p]

onExpM :: Monad m => (Exp -> m Exp) -> Prog -> m Prog
onExpM f = mapM (\(Func g args rhs) ->
             do rhs' <- f rhs
                return (Func g args rhs'))

fromExp :: (Exp -> [a]) -> Prog -> [a]
fromExp f p = concat [f rhs | Func g args rhs <- p]

instance Descend Exp where
  descendM f (App e es) = return App `ap` f e `ap` mapM f es
  descendM f (Case e as) = return Case `ap` f e `ap` mapM g as
    where g (p, e) = return (,) `ap` return p `ap` f e
  descendM f (Let bs e) = return Let `ap` mapM g bs `ap` f e
    where g (v, e) = return (,) `ap` return v `ap` f e
  descendM f (PrimApp p es) = return (PrimApp p) `ap` mapM f es
  descendM f (Lam vs e) = return (Lam vs) `ap` f e
  descendM f e = return e

subst :: Exp -> Id -> Exp -> Exp
subst x v = sub
  where
    sub (Var w) | v == w = x
    sub (Let bs e) | v `elem` map fst bs = Let bs e
    sub (Case e as) = Case (sub e)
                           [ (p, if v `elem` patVars p then e else sub e)
                           | (p, e) <- as ]
    sub (Lam vs e) = if v `elem` vs then Lam vs e else Lam vs (sub e)
    sub e = descend sub e

substMany :: Exp -> [(Exp, Id)] -> Exp
substMany = foldr (uncurry subst)

patVars :: Pat -> [Id]
patVars (App e es) = concatMap patVars (e:es)
patVars (Var v) = [v]
patVars p = []

caseAlts :: Exp -> [[Alt]]
caseAlts (Case exp alts) = alts : caseAlts exp ++ rest
  where rest = concatMap (caseAlts . snd) alts
caseAlts e = extract caseAlts e

freeVarsExcept :: [Id] -> Exp -> [Id]
freeVarsExcept vs e = nub (freeVarsExcept' vs e)

freeVarsExcept' :: [Id] -> Exp -> [Id]
freeVarsExcept' vs e = fv vs e
  where
    fv vs (Case e as) =
      fv vs e ++ concat [fv (patVars p ++ vs) e | (p, e) <- as]
    fv vs (Let bs e) = let ws = map fst bs ++ vs
                       in  fv ws e ++ concatMap (fv ws . snd) bs
    fv vs (Var w) = [w | w `notElem` vs]
    fv vs (Lam ws e) = fv (ws ++ vs) e
    fv vs e = extract (fv vs) e

freeVars :: Exp -> [Id]
freeVars e = nub (freeVarsExcept' [] e)

varRefs :: Id -> Exp -> Int
varRefs v = length . filter (== v) . freeVarsExcept' []

calls :: Exp -> [Id]
calls (Fun f) = [f]
calls e = extract calls e

lookupFuncs :: Id -> Prog -> [Decl]
lookupFuncs f p = [Func g args rhs | Func g args rhs <- p, f == g]

freshen :: Exp -> Fresh Exp
freshen (Let bs e) =
  do let (vs, es) = unzip bs
     e' <- freshen e
     es' <- mapM freshen es
     ws <- mapM (\_ -> fresh) vs
     let s = zip (map Var ws) vs
     return $ Let (zip ws (map (flip substMany s) es'))
                  (substMany e' s)
freshen (Case e as) = return Case `ap` freshen e `ap` mapM freshenAlt as
freshen e = descendM freshen e

freshenPat :: Pat -> Fresh Pat
freshenPat (Var _) = return Var `ap` fresh
freshenPat p = descendM freshenPat p

freshenAlt :: (Pat, Exp) -> Fresh (Pat, Exp)
freshenAlt (p, e) =
  do p' <- freshenPat p
     e' <- freshen e
     let s = zip (map Var (patVars p')) (patVars p)
     return (p', substMany e' s)

freshBody :: ([Id], Exp) -> Fresh ([Id], Exp)
freshBody (vs, e) =
  do ws <- mapM (\_ -> fresh) vs
     e' <- freshen e
     let s = zip (map Var ws) vs
     return (ws, substMany e' s)

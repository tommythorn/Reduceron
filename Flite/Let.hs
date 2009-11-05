module Flite.Let(inlineLinearLet, inlineSimpleLet, liftLet) where

import Flite.Syntax
import Flite.Traversals
import Flite.Descend
import Flite.Fresh
import Data.List

mkLet :: [Binding] -> Exp -> Exp
mkLet [] e = e
mkLet bs e = Let bs e

inlineLetWhen :: ([Binding] -> Exp -> Binding -> Bool) -> Prog -> Fresh Prog
inlineLetWhen f p = freshenLet p >>= return . onExp inline
  where
    inline (Let bs e) = mkLet (zip vs1 (map inline es1')) (inline e')
      where (vs, es) = unzip bs
            (bs0, bs1) = partition (f bs e) bs
            (vs1, es1) = unzip bs1
            (e':es1') = foldr (\(v, e) -> map (subst e v)) (e:es1) bs0
    inline e = descend inline e

inlineLinearLet :: Prog -> Fresh Prog
inlineLinearLet = inlineLetWhen linear
  where
    linear bs e (v, _) = refs v (e:map snd bs) <= 1
    refs v es = sum (map (varRefs v) es)

inlineSimpleLet :: Prog -> Fresh Prog
inlineSimpleLet = inlineLetWhen simple
  where
    simple _ _ (_, rhs) = simp rhs
    simp (App e []) = simp e
    simp (App e es) = False
    simp (Case e as) = False
    simp _ = True

freshenLet :: Prog -> Fresh Prog
freshenLet = onExpM freshen
  where
    freshen (Let bs e) =
      do let (vs, es) = unzip bs
         e1 <- freshen e
         es1 <- mapM freshen es
         ws <- mapM (\_ -> fresh) vs
         let e2:es2 = foldr (\(v, w) -> map (subst (Var w) v))
                            (e1:es1) (zip vs ws)
         return (mkLet (zip ws es2) e2)
    freshen e = descendM freshen e

liftLet :: Prog -> Fresh Prog
liftLet p = do p' <- freshenLet p
               return (onExp lift p')
  where
    lift e = mkLet [(v, liftInCase rhs) | (v, rhs) <- binds e]
                   (liftInCase (dropBinds e))

    liftInCase (Case e as) = Case e [(p, lift e) | (p, e) <- as]
    liftInCase e = descend liftInCase e

    dropBinds (Let bs e) = dropBinds e
    dropBinds (Case e as) = Case (dropBinds e) as
    dropBinds e = descend dropBinds e

    binds (Let bs e) = binds e ++ [(v, dropBinds e) | (v, e) <- bs]
                               ++ concatMap (binds . snd) bs
    binds (Case e as) = binds e
    binds e = extract binds e

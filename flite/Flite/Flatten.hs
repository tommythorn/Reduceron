module Flite.Flatten (flatten) where

import Flite.Syntax
import Flite.WriterState
import Data.List
import Flite.Traversals
import Control.Monad

expToApp :: Exp -> App
expToApp (App e es) = e:es
expToApp e = [e]

type Flatten a = WriterState (Id, App) Int a

intToId :: Int -> Id
intToId i = "tmp_" ++ show i

fresh :: Flatten Id
fresh = do { i <- get ; set (i+1) ; return (intToId i) }

flatten :: Exp -> [(Id, App)]
flatten e
  | length vs /= length (nub vs) = error "Flatten: the impossible happened"
  | otherwise = (intToId i, spine) : binds
  where
    (i, binds, spine) = runWS (flattenSpine e) 0

    vs = map fst binds

flattenSpine :: Exp -> Flatten App
flattenSpine (App e es) = mapM flattenExp (e:es)
flattenSpine (PrimApp p es) = return (Prim p:) `ap` mapM flattenExp es
flattenSpine (Let bs e) =
  do (bs', e') <- freshLet (bs, e)
     let (vs, es) = unzip bs'
     mapM flattenSpine es >>= mapM write . zip vs
     flattenSpine e'
flattenSpine e = (:[]) `fmap` flattenExp e

flattenExp :: Exp -> Flatten Exp
flattenExp (App e es) =
  do i <- fresh
     app <- mapM flattenExp (e:es)
     write (i, app)
     return (Var i)
flattenExp (PrimApp p es) =
  do i <- fresh
     app <- mapM flattenExp es
     write (i, Prim p:app)
     return (Var i)
flattenExp (Let bs e) =
  do (bs', e') <- freshLet (bs, e)
     let (vs, es) = unzip bs'
     mapM flattenSpine es >>= mapM write . zip vs
     flattenExp e'
flattenExp e = return e

freshLet :: ([Binding], Exp) -> Flatten ([Binding], Exp)
freshLet (bs, e) =
  do ws <- mapM (\_ -> fresh) vs
     let s = zip (map Var ws) vs
     let e' = substMany e s
     let es' = map (flip substMany s) es
     return (zip ws es', e')
  where
    (vs, es) = unzip bs

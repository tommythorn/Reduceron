module Flite.ConcatApp where

import Flite.Syntax
import Flite.Traversals
import Flite.Descend

concatApps :: Prog -> Prog
concatApps = onExp conc
  where
    conc (App e []) = conc e
    conc (App (App f xs) ys) = descend conc (App f (xs ++ ys))
    conc e = descend conc e

concatNonPrims :: Prog -> Prog
concatNonPrims = onExp conc
  where
    conc (App e []) = conc e
    conc (App (Fun f) xs) | isPrimId f = App (Fun f) (map conc xs)
    conc (App (App (Fun f) xs) ys) | isPrimId f =
      App (App (Fun f) (map conc xs)) (map conc ys)
    conc (App (App f xs) ys) = descend conc (App f (xs ++ ys))
    conc e = descend conc e

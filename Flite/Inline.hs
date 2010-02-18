module Flite.Inline (InlineFlag(..), inline, inlineTop) where

import Flite.Syntax
import Flite.Traversals
import Flite.ConcatApp
import Flite.Descend
import Flite.Fresh
import Flite.Dependency
import Control.Monad
import Flite.Let

data InlineFlag = NoInline | InlineAll | InlineSmall Int

checkInline :: InlineFlag -> Int -> Bool
checkInline NoInline n = False
checkInline InlineAll n = True
checkInline (InlineSmall bound) n = n <= bound

inlineTop :: InlineFlag -> Prog -> Fresh Prog
inlineTop NoInline p = return p
inlineTop i p = inline i p
            >>= inlineLinearLet
            >>= inlineSimpleLet

-- In-line saturated applications of small, non-recursive functions
inline :: InlineFlag -> Prog -> Fresh Prog
inline i p = onExpM inl p
  where
    cg = closure (callGraph p)
    inl (Fun f)
      | f `notElem` depends cg f =
        case lookupFuncs f p of
          Func f [] rhs:_ | checkInline i (numApps rhs) -> inl rhs
          _ -> return (Fun f)
    inl (App (Fun f) es)
      | f `notElem` depends cg f =
        case lookupFuncs f p of
          Func f args rhs:_
            | length args <= length es
           && checkInline i (numApps rhs) ->
                do let vs = map (\(Var v) -> v) args
                   ws <- mapM (\_ -> fresh) vs
                   let rhs' = substMany rhs (zip (map Var ws) vs)
                   inl (mkApp (mkLet (zip ws es) rhs') (drop (length vs) es))
          _ -> liftM (mkApp (Fun f)) (mapM inl es)
    inl e = descendM inl e


{-
-- In-line saturated applications of small, non-primitive functions
-- that do not have directly recursive definitions.  Does not inline a
-- function within an expression in which that function has already
-- been inlined.

inline :: InlineFlag -> Prog -> Fresh Prog
inline i p = onExpM (inl []) p
  where
    inl tabu (Fun f)
      | f `notElem` tabu =
        case lookupFuncs f p of
          Func f [] rhs:_ | checkInline i (numApps rhs) -> inl (f:tabu) rhs
          _ -> return (Fun f)
    inl tabu (App (Fun f) es)
      | f `notElem` tabu =
        case lookupFuncs f p of
          Func f args rhs:_
            | f `notElem` calls rhs
           && length args <= length es
           && checkInline i (numApps rhs) ->
                do let vs = map (\(Var v) -> v) args
                   ws <- mapM (\_ -> fresh) vs
                   let rhs' = substMany rhs (zip (map Var ws) vs)
                   inl (f:tabu)
                       (mkApp (mkLet (zip ws es) rhs') (drop (length vs) es))
          _ -> liftM (mkApp (Fun f)) (mapM (inl tabu) es)
    inl tabu e = descendM (inl tabu) e
-}

mkApp f [] = f
mkApp f es = App f es

mkLet [] e = e
mkLet bs e = Let bs e

numApps (App f xs) = 1 + sum (map numApps (f:xs))
numApps (Let bs e) = sum (map numApps (e:map snd bs))
numApps (Case e as) = max 1 (numApps e) + sum (map (numApps . snd) as)
numApps e = 0;

module Flite.InterpFrontend (frontend) where

import Flite.LambdaLift
import Flite.Syntax
import Flite.Traversals
import Flite.ConcatApp
import Flite.Matching
import Flite.Case
import Flite.Let
import Flite.Identify
import Flite.Inline
import Flite.Fresh
import Control.Monad

frontend :: InlineFlag -> Prog -> Prog
frontend i p = snd (runFresh (frontendM i p) "$" 0)

frontendM :: InlineFlag -> Prog -> Fresh Prog
frontendM i p =
      return (identifyFuncs p)
  >>= desugarCase
  >>= desugarEqn
  >>= inlineLinearLet
  >>= inlineSimpleLet
  >>= return . caseElim
  >>= return . concatApps
  >>= return . lambdaLift 'A'
  >>= inlineTop i
  >>= liftLet
  >>= return . finalPass

finalPass :: Prog -> Prog
finalPass = map freshen
  where
    freshen (Func f args rhs) = Func f (map Var args') (mkLet bs' e')
      where n = length args
            args' = map (('v':) . show) [0..n-1]
            (bs, e) = body rhs
            (vs, es) = unzip bs
            ws = map (('v':) . show) [n..n+length vs-1]
            from = map var args ++ vs
            to = args' ++ ws
            (e':es') = foldr (\(v, w) -> map (subst (Var w) v))
                             (e:es) (zip from to)
            bs' = zip ws es'

    var (Var v) = v

    body (Let bs e) = (bs, e)
    body e = ([], e)

    mkLet [] e = e
    mkLet bs e = Let bs e

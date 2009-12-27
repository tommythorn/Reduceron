module Flite.CompileFrontend (frontend) where

import Flite.Syntax
import Flite.Traversals
import Flite.Matching
import Flite.Case
import Flite.LambdaLift
import Flite.Let
import Flite.Identify
import Flite.Strictify
import Flite.ConcatApp
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
  >>= return . strictifyPrim

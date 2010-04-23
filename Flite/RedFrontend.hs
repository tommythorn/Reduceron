module Flite.RedFrontend (frontend) where

import Flite.LambdaLift
import Flite.Syntax
import Flite.Traversals
import Flite.ConcatApp
import Flite.Matching
import Flite.Case
import Flite.Let
import Flite.Identify
import Flite.Strictify
import Flite.Inline
import Flite.Predex
import Flite.Fresh
import Flite.WorkerWrapper
import Control.Monad
import Flite.Pretty

frontend :: Bool -> Int -> InlineFlag -> Prog -> Prog
frontend strictAnan nregs i p =
  snd (runFresh (frontendM strictAnan nregs i p) "$" 0)

frontendM :: Bool -> Int -> InlineFlag -> Prog -> Fresh Prog
frontendM strictAnan nregs i p =
  do p0 <- desugarCase (identifyFuncs p) >>= desugarEqn
     let sii = strictIntInfo p0
     p1 <- inlineLinearLet (concatApps p0)
             >>= inlineSimpleLet
             >>= return . lambdaLift 'A'
             >>= return . concApps nregs
             >>= (\p -> return (if strictAnan then workerWrapper sii p else p))
             >>= return . concApps nregs
             -- >>= inlineTop (InlineSmall 1)
             >>= return . concApps nregs
             >>= return . caseElimWithCaseStack
             >>= return . concApps nregs
             >>= inlineTop i
             >>= return . concApps nregs
             >>= return . identifyPredexCandidates nregs
             >>= return . concatApps
             >>= return . strictifyPrim
             >>= return . concatApps
     return p1

                 
concApps :: Int -> Prog -> Prog
concApps 0 = concatApps
concApps nregs = concatNonPrims

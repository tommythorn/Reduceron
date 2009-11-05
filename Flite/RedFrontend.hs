module Flite.RedFrontend (frontend) where

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
import Control.Monad
import Flite.Pretty

frontend :: Int -> InlineFlag -> Prog -> Prog
frontend nregs i p = snd (runFresh (frontendM nregs i p) "$" 0)

concApps :: Int -> Prog -> Prog
concApps 0 = concatApps
concApps nregs = concatNonPrims

frontendM :: Int -> InlineFlag -> Prog -> Fresh Prog
frontendM nregs i p =
      return (identifyFuncs p)
  >>= desugarCase
  >>= desugarEqn
  >>= inlineLinearLet
  >>= inlineSimpleLet
  >>= return . concApps nregs
  >>= inlineTop i
  >>= return . concApps nregs
 >>= inlineLinearLet
 >>= inlineSimpleLet
 >>= return . concApps nregs 
 -- >>= return . forceAndRebind
                  --  >>= return . identifyPredexCandidates nregs
  >>= return . caseElimWithCaseStack
  >>= inlineTop i
  >>= return . concApps nregs
  >>= return . identifyPredexCandidates nregs
  >>= return . concatApps
  >>= return . strictifyPrim
  >>= return . concatApps
--  >>= \p -> trace (pretty p) (return p)

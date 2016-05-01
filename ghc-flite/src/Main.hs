module Main where

import GHC (runGhc, compileToCoreSimplified, CoreModule, setSessionDynFlags, getSessionDynFlags, cm_binds)
import GHC.Paths (libdir)

import GhcMonad
import DynFlags
import CoreSyn
-- for printing
import GHC.IO.Handle.FD (stdout)
import Outputable (ppr, printForAsm)

import GHCFlite.Translate (translate)

import Flite.Pretty

main :: IO ()
main = do
    core <- runGhc (Just libdir) (coreModule "test/test_main.hs")
    print . translate $ cm_binds core
    return ()

-- | Sets up a session in the GhcMonad and
-- compiles the given file to a CoreModule
coreModule :: (GhcMonad m) => String -> m CoreModule
coreModule fileName = do
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags
    core <- compileToCoreSimplified fileName
    liftIO . printForAsm dflags stdout . ppr $ cm_binds core
    return core

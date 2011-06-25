module Fixit
  ( fixit
  )
 where

import Signal
import Netlist
import Generic
import Sequent
import Property
import Error
import LavaDir
import Verification

import List
  ( intersperse
  , nub
  )

import IO
  ( openFile
  , IOMode(..)
  , hPutStr
  , hClose
  )

import IOBuffering
  ( noBuffering
  )

import Data.IORef

import System
  ( system
  , ExitCode(..)
  )

----------------------------------------------------------------
-- fixit

fixit :: Checkable a => a -> IO ProofResult
fixit a =
  do checkVerifyDir
     noBuffering
     (props,_) <- properties a
     proveFile defsFile (writeDefinitions defsFile props)
 where
  defsFile = verifyDir ++ "/circuit.circ"

----------------------------------------------------------------
-- definitions

writeDefinitions :: FilePath -> [Signal Bool] -> IO ()
writeDefinitions file props =
  do han <- openFile file WriteMode
     var <- newIORef 0
     hPutStr han "INTERNALS{low := FALSE;}\n"
     hPutStr han "INTERNALS{high := TRUE;}\n"
     hPutStr han "LATCHES{initt := low (high);}\n"

     let new =
           do n <- readIORef var
              let n' = n+1
              writeIORef var n'
              return ("w" ++ show n')

         define v s =
           do hPutStr han (def ++ "\n")
          where
           def =
             case s of
               Bool True     -> prop $ "TRUE"
               Bool False    -> prop $ "FALSE"
               Inv x         -> prop $ "~" ++ x
               And xs        -> prop $ ops "&" xs "TRUE"
               Or  xs        -> prop $ ops "#" xs "FALSE"
               Xor xs        -> prop $ xorlist xs

               VarBool s     -> var s
               DelayBool x y -> latch x y
               _             -> wrong Error.NoArithmetic

           prop form =
             "INTERNALS{" ++ v ++ " := " ++ form ++ ";}"

           var s =
             prop s ++ " PSEUDOS{" ++ s ++ ";}"

           latch x y =
                "LATCHES{" ++ v ++ "_x := " ++ y ++ " (low);}\n"
             ++ "INTERNALS{" ++ v ++ " := (initt & " ++ x
             ++ ") # (~initt & " ++ v ++ "_x);}"

           ops op []  nul = nul
           ops op [x] nul = x
           ops op xs  nul =
             "(" ++ concat (intersperse (" " ++ op ++ " ") xs) ++ ")"

           xorlist []     = "FALSE"
           xorlist [x]    = x
           xorlist [x,y]  = x ++ " #! " ++ y
           xorlist (x:xs) = "(" ++ x_not_xs ++ ") # (" ++ not_x_xor_xs ++ ")"
            where
             x_not_xs     = x ++ concatMap (" & ~" ++) xs
             not_x_xor_xs = "~" ++ x ++ " & (" ++ xorlist xs ++ ")"

     ~(Compound vs) <- netlistIO new define (struct props)
     define "bad" (And [ "~" ++ v | Object v <- vs ])
     hClose han

----------------------------------------------------------------
-- primitive proving

proveFile :: FilePath -> IO () -> IO ProofResult
proveFile file before =
  do putStr "Fixit: "
     before
     putStr "... "
     system ("rm -f " ++ verifyDir ++ "/fixit.lock")
     lavadir <- getLavaDir
     x <- system ( lavadir
                ++ "/Scripts/fixit.wrapper "
                ++ file
                ++ " -showTime -dir=forward -sat=prove"
                 )
     let res = case x of
                 ExitSuccess   -> Valid
                 ExitFailure 1 -> Indeterminate
                 ExitFailure _ -> Falsifiable
     putStrLn (show res ++ ".")
     return res

----------------------------------------------------------------
-- the end.


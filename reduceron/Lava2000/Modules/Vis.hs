module Vis
  ( vis
  , writeVis
  , writeVisInput
  , writeVisInputOutput
  , equivCheckVisInput
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
-- vis

vis :: Checkable a => a -> IO ProofResult
vis a =
  do checkVerifyDir
     noBuffering
     (props,_) <- properties a
     proveFile defsFile $
       writeDefinitions defsFile "circuit"
         (Nothing :: Maybe ()) (delay high (andl props)) (var "good")
 where
  defsFile = verifyDir ++ "/circuit.mv"

----------------------------------------------------------------
-- write Vis

writeVis :: (Constructive a, Generic b) => String -> (a -> b) -> IO ()
writeVis name circ =
  do writeVisInput name circ (var "inp")

writeVisInput :: (Generic a, Generic b) => String -> (a -> b) -> a -> IO ()
writeVisInput name circ inp =
  do writeVisInputOutput name circ inp (symbolize "outp" (circ inp))

writeVisInputOutput :: (Generic a, Generic b)
                     => String -> (a -> b) -> a -> b -> IO ()
writeVisInputOutput name circ inp out =
  do writeItAll name inp (circ inp) out

writeItAll :: (Generic a, Generic b) => String -> a -> b -> b -> IO ()
writeItAll name inp out out' =
  do noBuffering
     putStr ("Writing to file \"" ++ file ++ "\" ... ")
     writeDefinitions file name (Just inp) out out'
     putStrLn "Done."
 where
  file = name ++ ".mv"

----------------------------------------------------------------
-- definitions

writeDefinitions :: (Generic a, Generic b)
                 => FilePath -> String -> Maybe a -> b -> b -> IO ()
writeDefinitions file name minp out out' =
  do firstHandle  <- openFile file1 WriteMode
     secondHandle <- openFile file2 WriteMode
     var <- newIORef 0

     hPutStr firstHandle $ unlines $
       [ ".model " ++ name
       ] ++
       [ ".inputs " ++ v
       | VarBool v <- inps
       ]

     hPutStr secondHandle $ unlines $
       [ ".outputs " ++ v
       | VarBool v <- outs'
       ] ++
       [ ".table  -> low"
       , "0"
       , ".latch low initt"
       , ".reset initt"
       , "1"
       ]

     let new =
           do n <- readIORef var
              let n' = n+1
                  v  = "w" ++ show n'
              writeIORef var n'
              return v

         define v s =
           case s of
             Bool True     -> port (\_   -> True)  []
             Bool False    -> port (\_   -> False) []
             Inv x         -> port (\[p] -> not p) [x]

             And []        -> define v (Bool True)
             And [x]       -> port (\[p]   -> p)      [x]
             And [x,y]     -> port (\[p,q] -> p && q) [x,y]
             And (x:xs)    -> define (w 0) (And xs)
                           >> define v (And [x,w 0])

             Or  []        -> define v (Bool False)
             Or  [x]       -> port (\[p]   -> p)      [x]
             Or  [x,y]     -> port (\[p,q] -> p || q) [x,y]
             Or  (x:xs)    -> define (w 0) (Or xs)
                           >> define v (Or [x,w 0])

             Xor  []       -> define v (Bool False)
             Xor  [x]      -> port (\[p]   -> p)      [x]
             Xor  [x,y]    -> port (\[p,q] -> p /= q) [x,y]
             Xor  (x:xs)   -> define (w 0) (Or xs)
                           >> define (w 1) (Inv (w 0))
                           >> define (w 2) (And [x, w 1])

                           >> define (w 3) (Inv x)
                           >> define (w 4) (Xor xs)
                           >> define (w 5) (And [w 3, w 4])
                           >> define v     (Or [w 2, w 5])

             VarBool s     -> do port (\[p] -> p) [s]
                                 case (minp,s) of
                                   (Nothing, 'i':_) -> input s
                                   _ -> return ()
             DelayBool x y -> delay x y

             _             -> wrong Error.NoArithmetic
           where
            w i = v ++ "_" ++ show i

            input s =
              do hPutStr firstHandle $
                   ".inputs " ++ s ++ "\n"

            port oper args =
              do hPutStr secondHandle $
                      ".table "
                   ++ unwords args
                   ++ " -> "
                   ++ v ++ "\n"
                   ++ unlines
                      [ line (xs ++ [oper xs])
                      | xs <- binary (length args)
                      ]
             where
              line bs =
                unwords (map (\b -> if b then "1" else "0") bs)

              binary 0 = [[]]
              binary n = map (False:) xs ++ map (True:) xs
               where
                xs = binary (n-1)

            delay x y =
              do hPutStr secondHandle $ unlines
                    [ ".latch " ++ y ++ " " ++ v ++ "_x"
                    , ".reset " ++ v ++ "_x"
                    , "0"
                    , ".table initt " ++ x ++ " " ++ v ++ "_x -> " ++ v
                    , "1 - - =" ++ x
                    , "0 - - =" ++ v ++ "_x"
                    ]

     outvs <- netlistIO new define (struct out)

     sequence
       [ define v' (VarBool v)
       | (v,v') <- flatten outvs `zip` [ v' | VarBool v' <- outs' ]
       ]

     hPutStr secondHandle $ unlines $
       [ ".end"
       ]

     hClose firstHandle
     hClose secondHandle
     system ("cat " ++ file1 ++ " " ++ file2 ++ " > " ++ file)
     system ("rm " ++ file1 ++ " " ++ file2)
     return ()
 where
  file1 = file ++ "_1"
  file2 = file ++ "_2"

  sigs x = map unsymbol . flatten . struct $ x

  inps  = case minp of
            Just inp -> sigs inp
            Nothing  -> []
  outs' = sigs out'

  make n s = take (n `max` length s) (s ++ repeat ' ')

----------------------------------------------------------------
-- equivalence checking

equivCheckVisInput circ1 circ2 inp =
  do checkVerifyDir
     noBuffering
     writeVisInput name1 circ1 inp
     writeVisInput name2 circ2 inp
     putStr "Vis: ... "
     lavadir <- getLavaDir
     x <- system ( lavadir
                ++ "/Scripts/vis.wrapper "
                ++ name1 ++ " " ++ name2
                ++ " -showTime"
                 )
     let res = case x of
                 ExitSuccess   -> Valid
                 ExitFailure 1 -> Indeterminate
                 ExitFailure _ -> Falsifiable
     putStrLn (show res ++ ".")
     return res
 where
  name  = "Verify/circuit"
  name1 = name ++ "_1"
  name2 = name ++ "_2"

----------------------------------------------------------------
-- primitive proving

proveFile :: FilePath -> IO () -> IO ProofResult
proveFile file before =
  do putStr "Vis: "
     before
     putStr "... "
     lavadir <- getLavaDir
     x <- system ( lavadir
                ++ "/Scripts/vis-reach.wrapper "
                ++ file
                ++ " -showTime"
                 )
     let res = case x of
                 ExitSuccess   -> Valid
                 ExitFailure 1 -> Indeterminate
                 ExitFailure _ -> Falsifiable
     putStrLn (show res ++ ".")
     return res

----------------------------------------------------------------
-- the end.


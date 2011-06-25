module Isc
 ( IscMethod(..)
 , isc
 , iscWith
 ) where

import Ref
import Signal
import Generic

import Lava
import LavaDir
import Sequent
import Netlist
import Verification
import IOBuffering

import Array
import MyST
import List(intersperse)
import System(system, ExitCode(..))

----------------------------------------------------------------------------------------------------
-- options

data IscMethod
 = StepMin
 | StepMax
 | Mixed
 | Bmc

----------------------------------------------------------------------------------------------------
-- toplevel proving

isc :: Checkable a => a -> IO ProofResult
isc = iscWith Mixed 10

iscWith :: Checkable a => IscMethod -> Int -> a -> IO ProofResult
iscWith m n a =
  do checkVerifyDir
     noBuffering
     net_ netfile a
     lavadir <- getLavaDir
--     r <- system (lavadir ++ execute)
     r <- system (execute)
     return $ case r of
       ExitSuccess   -> Valid
       ExitFailure 1 -> Indeterminate
       ExitFailure _ -> Falsifiable

 where
--  execute = "/Scripts/isc "
  execute = "isc "
	    ++ netfile ++ " " 
	    ++ (m2par m) ++ " " ++ show n
  netfile = verifyDir ++ "/circuit.net"

  m2par StepMin = "-smin"
  m2par StepMax = "-smax"
  m2par Mixed   = "-mix"
  m2par Bmc     = "-bmc"

----------------------------------------------------------------------------------------------------
-- netlist

data Sign a  = Pos a | Not a deriving (Show, Read)

sneg (Pos x) = Not x
sneg (Not x) = Pos x 

type NetList = Array Int (S (Sign Int))

instance Functor Sign where
  fmap f (Pos x) = Pos (f x)
  fmap f (Not x) = Not (f x)


showS :: S String -> String
showS (Bool b)     = "Bool " ++ show b
showS (Or [x,y])   = "Or " ++ "[" ++ x ++ ',' : y ++ "]"
showS (Xor [x,y])  = "Xor " ++ "[" ++ x ++ ',' : y ++ "]"
showS (VarBool v)  = "VarBool " ++ show v
showS (DelayBool x y) = "DelayBool (" ++ x ++ ") (" ++ y ++ ")"
showS _               = error "showS"

showN :: NetList -> String
showN = showA . fmap (showS . fmap show)

showA :: Array Int String -> String
showA a = "array " ++ show (bounds a) ++ " [" ++
	  concat (intersperse "," (map (\ (i,s) -> "(" ++ show i ++ "," ++ s ++ ")")
		                         (assocs a)))
	  ++ "]"

showAll (n,p) = "(" ++ showN n ++ "," ++ show p ++ ")"

net_ :: Checkable a => String -> a -> IO ()
net_ file a = do
  p <- net a
  writeFile file (showAll p)

net :: Checkable a => a -> IO (NetList, Sign Int)
net a =
  do (props,_) <- properties a
     let top = case props of
                [x] -> x
		xs  -> andl xs
     let (tab, Object top') = table (struct top)
     return (array (0, length tab-1) tab, top')


table :: Sequent f => f Symbol -> ([(Int,S (Sign Int))], f (Sign Int))
table str =
  runST
  ( do ref   <- newSTRef 0
       table <- newSTRef []

       let define sym = do
             tab <- readSTRef table
	     case sym of
	       And xs -> bin Or (map sneg xs) >>= return . sneg
	       Or xs  -> bin Or xs
	       Xor xs -> bin Xor xs
--			    v <- new
--			    writeSTRef table ((v,x):tab)
--			    return (Not v)
	       Inv x  -> return (sneg x)
	       x      -> do v <- new
			    writeSTRef table ((v,x):tab)
			    return (Pos v)

           bin f [x]      = return x
	   bin f (x:y:xs) = do
               v <- new
	       tab <- readSTRef table
	       writeSTRef table ((v, f [x,y]) : tab)
	       bin f (Pos v:xs)

           new = do
	     n <- readSTRef ref
	     writeSTRef ref (n+1)
	     return n

       str' <- netlistST_ define str
       tab  <- readSTRef table
       return (tab, str')
  )

netlistST_ :: Sequent f => (S v -> ST s v) -> f Symbol -> ST s (f v)
netlistST_ define symbols =
  do tab <- tableST
     
     let gather (Symbol sym) =
           do visited <- findST tab sym
              case visited of
                Just v  -> do return v
                Nothing -> fixST $ \v -> do
                              extendST tab sym v
                              s <- mmap gather (deref sym)
                              define s
           
      in mmap gather symbols


{-
isLow  (Bool False) = True
isLow  (Inv s)      = isHigh (unsymbol s)
isLow  _            = False
isHigh (Bool True)  = True
isHigh (Inv s)      = isLow (unsymbol s)
isHigh _            = False
-}

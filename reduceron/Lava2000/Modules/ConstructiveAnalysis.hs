module ConstructiveAnalysis where

import Signal
import Operators
import Error
import Generic
import Sequent
import Ref
  
import MyST
  ( STRef
  , newSTRef
  , readSTRef
  , writeSTRef
  , runST
  , fixST
  , unsafeInterleaveST
  )

import List
  ( isPrefixOf
  )

----------------------------------------------------------------
-- constructive analysis

constructive :: (Generic a, Generic b) => (a -> b) -> (a -> Signal Bool)
constructive circ inp =
  runST
  ( do defined <- newSTRef []
       table   <- tableST
       
       let gather (Symbol sym) =
             do ms <- findST table sym
                case ms of
                  Just s  -> do return s
                  Nothing -> fixST (\s ->
                                do extendST table sym s
                                   ss <- mmap (unsafeInterleaveST . gather) (deref sym)
                                   define ss
                             )

           define (Bool b) =
             do return (bool b, bool (not b))

           define (DelayBool ~(inipos,inineg) ~(nextpos,nextneg)) =
             do return (respos, inv respos)
            where
             respos = delay inipos nextpos
                
           define (VarBool s) =
             do return (respos, inv respos)
            where
             respos
               | tag `isPrefixOf` s = Signal (pickSymbol (drop (length tag) s) inp)
               | otherwise          = var s

           define (Inv ~(xpos,xneg)) =
             do result ( andl [xneg]
                       , orl  [xpos]
                       )
           
           define (And xs) =
             do result ( andl [ xpos | (xpos,_) <- xs ]
                       , orl  [ xneg | (_,xneg) <- xs ]
                       )
             
           define (Or xs) =
             do result ( orl  [ xpos | (xpos,_) <- xs ]
                       , andl [ xneg | (_,xneg) <- xs ]
                       )

           define (Xor xs) =
             do result ( xorpos xs
                       , xorneg xs
                       )
            where
             xorpos []               = low
             xorpos [(xpos,xneg)]    = xpos
             xorpos ((xpos,xneg):xs) =
               or2 ( andl (xpos : [ xneg | (_,xneg) <- xs ])
                   , andl [ xneg, xorpos xs ] 
                   )
            
             xorneg xs =
               or2 ( andl [ xneg | (_,xneg) <- xs ]
                   , orl  [ and2 (xpos,ypos)
                          | (xpos,ypos) <- pairs [ xpos | (_,xpos) <- xs ]
                          ]
                   )
            
             pairs []     = []
             pairs (x:xs) = [ (x,y) | y <- xs ] ++ pairs xs

           define s =
             do wrong NoArithmetic

           result (xpos,xneg) =
             do defs <- readSTRef defined
                writeSTRef defined ((xpos <#> xneg) : defs)
                return (xpos,xneg)
                
        in mmap gather (struct (circ (symbolize tag inp)))
       
       defs <- readSTRef defined
       return (andl defs)
  )
 where
  tag = "#constr#"

----------------------------------------------------------------
-- the end.


module Compiler.Compile where

import Yhc.Core
import Data.Generics.Play
import Data.List
import Compiler.Prog
import Compiler.State
import Data.List
import Debug.Trace

data Op = Add | Sub | Eq | NotEq | LessEq deriving Show

data CombNode =
    Start Int Int Int -- first node of combinator: num args, size, spine len
  | Int Int           -- integer literal
  | Addr Int          -- appliction node: pointer to a sequence of nodes
  | End CombNode      -- last node in sequence
  | PrimOp Op         -- primitive
  | Fun String        -- a combinator, with a name (intermediate)
  | FunInt Int        -- a combinator, with an address
  | Arg Int           -- a reference to an argument in a combinator
    --deriving Show

instance Show CombNode where
  show (Start i j k) = "Start " ++ unwords [show i, show j, show k]
  show (Int i) = "Int " ++ show i
  show (End n) = "End (" ++ show n ++ ")"
  show (Addr a) = "Ap " ++ show a
  show (FunInt i) = "Fun " ++ show i
  show (Arg i) = "Var " ++ show i
  show (PrimOp p) = "Prim " ++ case p of
                                 Add -> "PLUS"
                                 Sub -> "MINUS"
                                 Eq -> "EQUAL"
                                 LessEq -> "LESSEQ"
                                 NotEq -> "NOTEQ"

type Bindings = [(String, [CoreExpr])]

type BindingsM a = State (Int, Bindings) a

newName :: BindingsM String
newName = do (i, bs) <- get ; put (i+1, bs) ; return ("_temp" ++ show i)

isTemp :: String -> Bool
isTemp v = "_temp" `isPrefixOf` v

addBinding :: String -> [CoreExpr] -> BindingsM ()
addBinding v es = modify (\(i, bs) -> (i, (v, es):bs))

rename :: String -> String -> BindingsM ()
rename v w = modify (\(i, bs) -> (i, ren bs))
  where
    ren = map (\(s, e) -> if s == v then (w, e) else (s, e))

bindingsM :: CoreExpr -> BindingsM CoreExpr
bindingsM = traverseM f
  where
    f (CoreApp a as)   = do n <- newName
                            addBinding n (a:as)
                            return (CoreVar n)
    f (CoreLet bs e)   = mapM bind bs >> return e
    f e                = return e

    bind (v, CoreVar w)
      | isTemp w        = rename w v
    bind (v, e)         = addBinding v [e]
                          
bindings :: Bool -> CoreExpr -> Bindings
bindings spineFirst e = move "_top" (snd bs)
  where
    e' = CoreLet [("_top", e)] (CoreVar "_top")
    (bs, e'') = runState (bindingsM e') (0, [])

    move f p = if spineFirst then [a] ++ bs else bs ++ [a]
      where
        ([a], bs) = partition ((== f) . fst) p


toNode :: [String] -> [(String, Int)] -> CoreExpr -> CombNode
toNode vs os (CoreVar v) =
  case elemIndex v vs of
    Just i  -> Arg i
    Nothing -> case lookup v os of
                 Just i -> Addr i
toNode vs os (CoreFun f)
  | f == "ADD_W" = PrimOp Add 
  | f == "SUB_W" = PrimOp Sub
  | f == "EQ_W"  = PrimOp Eq
  | f == "NE_W"  = PrimOp NotEq
  | f == "LE_W"  = PrimOp LessEq
  | otherwise    = Fun f
toNode vs os (CoreInt i) = Int i
toNode vs os (CoreInteger i) = Int (fromInteger i)
toNode vs os (CoreChr _) = warnType "Char" (Int 0)
toNode vs os (CoreStr _) = warnType "Char" (Int 0)
toNode vs os (CoreFloat _) = warnType "Float" (Int 0)
toNode vs os (CoreDouble _) = warnType "Double" (Int 0)

warnType x y = trace ("WARNING: " ++ x ++ " not supported.") y

comp :: Bool -> Bool -> [String] -> CoreExpr -> [CombNode]
comp revAps spineFirst args e =
  Start (length args)
        (length ns)
        (case spineFirst of
           False -> length $ snd $ last bs
           True  -> (length $ snd $ head bs) - 1) : ns
  where
    bs      = bindings spineFirst e
    sizes   = map (length . snd) bs
    offsets = zip (map fst bs) (scanl (+) 1 sizes)
    ns      = concatMap (markEnd . (if revAps then reverse else id) .
                           map (toNode args offsets) . snd) bs
    
    markEnd xs = init xs ++ [End (last xs)]

compile :: Prog -> Bool -> Bool -> [CombNode]
compile p revAps spineFirst = ns
  where
    bs      = map (\(s, (vs, e)) -> (s, comp revAps spineFirst vs e)) p
    sizes   = map (length . snd) bs
    offsets = zip (map fst bs) (scanl (+) 0 sizes)
    ns      = concatMap (map trFun . snd) bs

    trFun (Fun f) = case lookup f offsets of
                      Nothing -> if isPrim f
                                   then FunInt 0
                                   else warnFun f (FunInt 0)
                      Just i  -> FunInt i
    trFun (End e) = End (trFun e)
    trFun e       = e

warnFun x y = trace ("WARNING: function " ++ x ++ " unrecognised.") y

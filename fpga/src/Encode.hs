module Encode where

import Data.Bits
import Syntax
import Data.List
import Data.Maybe
import qualified Bytecode as B
import Debug.Trace

encodeAtom :: Atom -> Integer
encodeAtom (INT i)   = B.encodeINT (fromIntegral i)
encodeAtom (ARG s i) = B.encodeARG s (fromIntegral i)
encodeAtom (REG s i) = B.encodeREG s (fromIntegral i)
encodeAtom (VAR s i) = B.encodeAP s (fromIntegral i)
encodeAtom (CON n i) = B.encodeCON (fromIntegral n) (fromIntegral i)
encodeAtom (FUN b n i) = B.encodeFUN b (fromIntegral n) (fromIntegral i)
encodeAtom (PRI n s)
  | s' == "(+)"  = B.primADD swap
  | s' == "(-)"  = B.primSUB swap
  | s' == "(==)" = B.primEQ swap
  | s' == "(/=)" = B.primNEQ swap
  | s' == "(<=)" = B.primLEQ swap
  | s' == "(.&.)" = B.primAND swap
  | s' == "st32" = B.primST32 swap
  | s' == "ld32" = B.primLD32 swap
  | otherwise   = error $ "Encode.encodeAtom: primitive '" ++ s ++ "' "
                       ++ "not yet supported on the Reduceron."
  where
    swap = "swap:" `isPrefixOf` s
    s' = if swap then drop 5 s else s

encodeApp :: App -> Integer
encodeApp (APP n as)
  | length as >= 1 && length as <= 4 =
    B.encodeApp (genericLength as - 1) n False (map encodeAtom as)
encodeApp (CASE alts as)
  | length as >= 1 && length as <= 3 =
    B.encodeApp (genericLength as - 1) False True (map encodeAtom bs)
  where bs = take 3 (as ++ repeat (INT 0)) ++ [INT alts]
encodeApp (PRIM _ as)
  | length as == 3 =
    B.encodeApp 2 False False (map encodeAtom as)
encodeApp a = error ("Encode.encodeApp: invalid arguments")

encodeTemplate :: Template -> Integer
encodeTemplate (_, popN, alts, pushes, apps)
  | length apps <= 2 = encodeTemp popN alts pushes app1' app2'
  | otherwise = error "encodeTemplate: invalid arguments"
  where
    app1 = if length apps > 0 then Just (apps !! 0) else Nothing
    app2 = if length apps > 1 then Just (apps !! 1) else Nothing
    (app1', app2') = swapPrimApps (app1, app2)

swapPrimApps (app1, app2)
  | isJust app1 && isJust app2
 && isPrim (fromJust app1) /= isPrim (fromJust app2)
  = error "encodeTemplate: invalid arguments"
  | isJust app1 && isPrim (fromJust app1)
  = if isEvenPrim (fromJust app1) then (app1, app2) else (app2, app1)
  | otherwise = (app1, app2)

isPrim :: App -> Bool
isPrim (PRIM _ _) = True
isPrim _ = False

isEvenPrim :: App -> Bool
isEvenPrim (PRIM n _) = even n

encodeTemp popN alts pushes app1 app2
  | 0 <= popN && popN <= 8
 && length alts <= 1
 && length pushes >= 1 && length pushes <= 6
 && (if length pushes > 1 then isNothing app2 else True)
  = B.encodeTemplate
      (fromIntegral (length pushes - 1 - popN))
      (encodeAtom $ head pushes)
      (length alts >= 1)
      (head ((map fromIntegral alts) ++ [0]))
      (isJust app2)
      (getHeader app2')
      (2^(genericLength pushes-1)-1)
      (if length pushes > 1 then pushes' else getAtoms app2')
      (isJust app1) --(isJust app1 || maybe False isPrim app2)
      (maybe 0 encodeApp app1)
      (maybe False isPrim app1)
      (maybe False isPrim app2)
      (maybe 0 encodeDestReg app1)
      (maybe 0 encodeDestReg app2)
  | otherwise = error "encodeTemp: invalid arguments"
  where
    app2' = maybe 0 encodeApp app2
    pushes' = B.joinIntegers B.atomWidth $ map encodeAtom $
                take 5 (tail pushes ++ repeat (INT 0))
    getHeader a = a .&. 31
    getAtoms a = a `shiftR` 5

encodeDestReg (PRIM r _) = 1 `shiftL` (r `div` 2)
encodeDestReg _ = 0

bytecode :: Prog -> [Integer]
bytecode = map encodeTemplate

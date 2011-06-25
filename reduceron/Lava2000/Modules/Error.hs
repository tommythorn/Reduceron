module Error
  ( Error(..)
  , wrong
  )
 where

----------------------------------------------------------------
-- Error

data Error
  = DelayEval
  | VarEval
  | CombinationalLoop
  | BadCombinationalLoop
  | UndefinedWire
  | IncompatibleStructures
  | NoEquality
  | NoArithmetic
  | EnumOnSymbols

  | Internal_OptionNotFound
 deriving (Eq, Show)

wrong :: Error -> a
wrong err =
  error $
    case err of
      DelayEval              -> "evaluating a delay component"
      VarEval                -> "evaluating a symbolic value"
      CombinationalLoop      -> "combinational loop"
      BadCombinationalLoop   -> "short circuit"
      UndefinedWire          -> "undriven output"
      IncompatibleStructures -> "combining incompatible structures"
      NoEquality             -> "no equality defined for this type"
      NoArithmetic           -> "arithmetic operations are not supported"
      EnumOnSymbols          -> "enumerating symbolic values"
      
      Internal_OptionNotFound -> internal "option not found"
      _                       -> internal "unknown error"

internal :: String -> String
internal msg = "INTERNAL ERROR: " ++ msg
            ++ ".\nPlease report this as a bug to `koen@cs.chalmers.se'."

----------------------------------------------------------------
-- the end.


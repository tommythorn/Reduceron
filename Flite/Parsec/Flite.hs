module Flite.Parsec.Flite (main) where

import Flite.Syntax
import Flite.Parsec.Parse
import Flite.Pretty
import Flite.Interp
import Flite.Inline
import Flite.Compile
import Flite.RedCompile
import Data.List
import System
import System.IO
import System.Console.GetOpt

data Flag =
    Desugar
  | CompileToC
  | CompileToRed Int Int Int Int Int
  | Inline (Maybe Int)
  | StrictnessAnalysis
  deriving Eq

isDisjoint (Inline i) = False
isDisjoint StrictnessAnalysis = False
isDisjoint flag = True

options :: [OptDescr Flag]
options =
  [ Option ['d'] [] (NoArg Desugar) "desugar"
  , Option ['c'] [] (NoArg CompileToC) "compile to C"
  , Option ['r'] [] (OptArg red "MAXPUSH:APSIZE:MAXAPS:MAXLUTS:MAXREGS")
                    "compile to Reduceron templates"
  , Option ['i'] [] (OptArg (Inline . fmap read) "MAXAPS")
                    "inline small function bodies"
  , Option ['s'] [] (NoArg StrictnessAnalysis) "employ strictness analysis"
  ]
  where
    redDefaults = CompileToRed 6 4 2 1 0
    red Nothing = redDefaults
    red (Just s) =
      case split ':' s of
        [a, b, c, d, e] ->
          CompileToRed (read a) (read b) (read c) (read d) (read e)
        _ -> error (usageInfo header options)

header = "Usage: Flite [OPTION...] FILE.hs"

main =
  do args <- getArgs
     case getOpt Permute options args of
       (flags, [fileName], []) -> run flags fileName
       (_, _, errs) -> error (concat errs ++ usageInfo header options)

run flags fileName =
  do p <- parseProgFile fileName
     let inlineFlag = head $ [InlineAll | Inline Nothing <- flags]
                          ++ [InlineSmall i | Inline (Just i) <- flags]
                          ++ [NoInline]
     case filter isDisjoint flags of
       [] -> interp inlineFlag p `seq` return ()
       [Desugar] ->
         putStrLn $ pretty $ frontend inlineFlag p
       [CompileToC] -> putStrLn $ compile inlineFlag p
       [CompileToRed slen alen napps nluts nregs] ->
        do let sa = StrictnessAnalysis `elem` flags
           mapM_ print $ redCompile inlineFlag sa slen alen napps nluts nregs p
       _ -> error (usageInfo header options)

-- Auxiliary

split :: Eq a => a -> [a] -> [[a]]
split x xs =
  case elemIndex x xs of
    Nothing -> [xs]
    Just i -> let (first, rest) = splitAt i xs in
                first : split x (dropWhile (== x) rest)

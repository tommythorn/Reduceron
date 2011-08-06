import Lava
import Recipe
import Reduceron
import Syntax
import Encode
import Monad
import Hex
import System
import System.Console.GetOpt

import Collect

import Heap

data Flag = Simulate | Generate | GenVerilog | Mem deriving Eq

options :: [OptDescr Flag]
options =
  [ Option ['s'] [] (NoArg Simulate) "simulate"
  , Option ['g'] [] (NoArg Generate) "generate VHDL"
  , Option ['v'] [] (NoArg GenVerilog) "generate Verilog"
  , Option ['m'] [] (NoArg Mem) "generate .mem file "
  ]

header = "Usage: Machine [OPTION...] FILE.red"

main =
  do args <- getArgs
     case getOpt Permute options args of
       (flags, [fileName], []) -> run flags fileName
       (_, _, errs) -> error (concat errs ++ usageInfo header options)

run flags fileName =
  do contents <- readFile fileName
     let prog = map read (lines contents)
     let code = bytecode prog
     when (null flags) $ putStrLn (usageInfo header options)
     when (Simulate `elem` flags) $
--     mapM_ print $ take 620 $ simRecipe2 (newReduceron code) dispatch
--                       (\s -> s!state)

       print $ simRecipe (newReduceron code) dispatch
                         (\s -> ( s!result!val
                                , s!heap!Heap.size ))

                                -- , s!collector!gcCount!val))
       --print $ simRecipe (newReduceron code) dispatch (Heap.size . heap)
     when (Generate `elem` flags) $
       let (r, fin) = recipe (newReduceron code) dispatch (delay high low)
       in  writeVhdl "Reduceron"
                     --(r!result!val, fin)
                     --(nameWord "result", name "finish")
                     (r!result!val, r!state, r!heap!Heap.size, fin)
                     (nameWord "result", nameWord "state", nameWord "heapSize", name "finish")
     when (GenVerilog `elem` flags) $
       let (r, fin) = recipe (newReduceron code) dispatch (delay high low)
       in  writeVerilog "Reduceron"
                     --(r!result!val, fin)
                     --(nameWord "result", name "finish")
                     (r!result!val, r!state, r!heap!Heap.size, fin)
                     (nameWord "result", nameWord "state", nameWord "heapSize", name "finish")
     when (Mem `elem` flags) $
       do putStr "@0 "
          putStrLn $ unwords (mem code)

mem :: [Integer] -> [String]
mem = concatMap f
  where
    f = map (reverse . binToHex)
      . map (++ [False, False])
      . map fixParity
      . groupN 18
      . pad
      . intToBin

    pad xs = take 234 (xs ++ repeat False)

    fixParity xs = take 8 xs ++ take 8 (drop 9 xs) ++ [xs !! 8, xs !! 17]

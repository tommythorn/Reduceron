import Prelude hiding (Word, (.))
import Lava
import Recipe
import Reduceron
import Syntax
import Encode
import Control.Monad
import Hex
import System.Environment
import System.Console.GetOpt

import Collect

import Heap

data Flag = Simulate | Generate | GenVerilog | GenC | Name String
     deriving Eq

options :: [OptDescr Flag]
options =
  [ Option ['s'] ["simulate"]       (NoArg Simulate)    "simulate"
  , Option ['g'] ["vhdl-output"]    (NoArg Generate)    "generate VHDL"
  , Option ['v'] ["verilog-output"] (NoArg GenVerilog)  "generate Verilog"
  , Option ['c'] ["c-output"]       (NoArg GenC)        "generate C"
  , Option ['o'] ["output-name"]    (ReqArg Name "NAME") "output name"
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
     let outName = head [n | Name n <- flags ++ [Name "Reduceron"]]
     when (null flags) $ putStrLn (usageInfo header options)
     when (Simulate `elem` flags) $
--     mapM_ print $ take 620 $ simRecipe2 (newReduceron code) dispatch
--                       (\s -> s.state)

       print $ simRecipe (newReduceron code) dispatch
                         (\s -> ( s.result.val
                                , s.heap.Heap.size ))

                                -- , s.collector.gcCount.val))
       --print $ simRecipe (newReduceron code) dispatch (Heap.size . heap)
     when (Generate `elem` flags) $
       let (r, fin) = recipe (newReduceron code) dispatch (delay high low)
       in  writeVhdl outName
                     --(r.result.val, fin)
                     --(nameWord "result", name "finish")
                     (r.result.val, r.state, r.heap.Heap.size,
                      r.ioWrite.val, r.ioAddr.val, r.ioWriteData.val,
                      fin)
                     (nameWord "result", nameWord "state", nameWord "heapSize",
                      nameWord "ioWrite", nameWord "ioAddr", nameWord "ioWriteData",
                      name "finish")
     when (GenVerilog `elem` flags) $
       let (r, fin) = recipe (newReduceron code) dispatch (delay high low)
       in  writeVerilog2 outName "Reduceron"
                     (r.result.val, r.state, r.heap.Heap.size,
                      r.ioAddr.val, r.ioWrite.val, r.ioRead.val, r.ioWriteData.val,
                      -- r.ioReadData.val, r.ioWait.val,
                      fin)
                     (nameWord "result", nameWord "state", nameWord "heapSize",
                      nameWord "ioAddr", nameWord "ioWrite", nameWord "ioRead", nameWord "ioWriteData",
                      -- nameWord "ioReadData", nameWord "ioWait",
                      name "finish")
     when (GenC `elem` flags) $
       let (r, fin) = recipe (newReduceron code) dispatch (delay high low)
       in  writeC outName
                     (r.result.val, fin)
                     (nameWord "result", name "done")

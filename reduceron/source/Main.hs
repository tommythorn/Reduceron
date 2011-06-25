module Main where

import Yhc.Core
import Data.Bits
import Compiler.Prepare
import Compiler.Prog
import Compiler.Compile
import Compiler.Interp
import Compiler.Bytecode
import System
import System.Console.GetOpt
import Red7.Generate
import Red12.Generate
import Red13.Generate
import Red14.Generate
import Red15.Generate
import Red16.Generate

main :: IO ()
main = do args <- getArgs
          case getOpt Permute options args of
            (fs, [fn], []) -> tred fn fs
            (_, _, errs)  -> error $ concat errs
                                  ++ usageInfo header options
  where
    header = "Usage: reduceron [OPTION]... FILE"

data Flag = OutFile String | Interpret | Version Int

options :: [OptDescr Flag]
options =
  [ Option ['o'] [] (ReqArg OutFile "NAME")
      "output file (default \"out\")"
  , Option ['v'] [] (ReqArg version "NUMBER")
      "which version to use (default: best)"
  , Option ['i'] ["interpret"] (NoArg Interpret)
      "interpret compiled program"
  ]
  where
    version str = Version (read str)

tred :: String -> [Flag] -> IO ()
tred fn fs = do core <- loadCore fn
                let prog = prepare core
                let nodes = compile prog (version /= 12) (version >= 14)
                let bc = encode nodes
                print nodes
                writeFile (outfile ++ ".red") (concatMap bytes bc)
                putStrLn ("Bytecode written to '" ++ outfile ++ ".red'")
                case version of
                  7  -> genRed7 outfile bc
                  12 -> genRed12 outfile bc
                  13 -> genRed13 outfile bc
                  14 -> genRed14 outfile bc
                  15 -> genRed15 outfile bc
                  16 -> genRed16 outfile bc
                  _  -> error $ "Version " ++ show version ++ "not compiled"
                if interpret then print (interp nodes) else return ()
  where
    outfile = head ([s | OutFile s <- fs] ++ ["Out"])
    version = head ([s | Version s <- fs] ++ [15])
    interpret = not (null [() | Interpret <- fs])

    bytes :: Int -> [Char]
    bytes i =  [toEnum a, toEnum b, toEnum c]
      where
        a = i .&. 0x000000ff
        b = (i .&. 0x0000ff00) `shiftR` 8
        c = (i .&. 0x00ff0000) `shiftR` 16

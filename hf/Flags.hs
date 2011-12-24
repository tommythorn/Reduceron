-----------------------------------------------------------------------
-- Flags and options for the hpc-trans program, part of HPC.
-- Based on similar modules in the hat tracing tools and nhc98 compiler
-- (see www.cs.york.ac.uk/fp/{nhc98,hat}).
-- Colin Runciman and Andy Gill, June 2006.
-----------------------------------------------------------------------

module Flags
  (Flags,processArgs,pF
  ,sUnix
  ,sUnlit
{-
  ,sSourceFile
-}
  ,sUnderscore
  ,sLex
  ,sParse
  ,sTraceFns
  ,sPrelude
{-
  ,sPreludes
  ,sIncludes
-}
  ,sDbgTrusted
{-
  ,sRealFile
-}
  ,sIBound
  ,sShowWidth
  ,sShowIndent
  ,sShowQualified
{-
  ,sHatAuxFile
  ,sHatTransFile
  ,sHatFileBase
  ,sSrcDir
  ,sHpcTransFile
  ,sHpcProgName
  ,sHpcMixFile
  ,sHpcHierModName
  ,sHpcLevel
  ,sHpcBoolCover
-}
  ) where

import IO
import OsOnly(fixRootDir,fixHatAuxFile,fixHatTransFile,fixHatTransDir,fixHatFileBase)
import List(isPrefixOf)
import Char(isDigit)


data Flags = FF 
  {
{- sRealFile   :: String
  ,sSourceFile :: String
  ,sHatAuxFile   :: String
  ,sHatTransFile :: String
  ,sHatFileBase  :: String
   sIncludes   :: [String]
  ,sPreludes   :: [String]
  ,sSrcDir     :: String
-}

--v Flags to control compilation
   sUnix       :: Bool	-- either unix or RiscOS
  ,sUnlit      :: Bool	-- unliterate the source code
  ,sPrelude    :: Bool	-- keep prelude defns in interface file

--v Flags to control compilation for tracing
  ,sDbgTrusted :: Bool	-- trust this module

--v Flags for machine architecture / configuration
  ,sUnderscore :: Bool	-- force H'98 underscores

--v debugging flags - show program / import tables (after each compiler phase)
  ,sLex        :: Bool	-- input	after lexing
  ,sParse      :: Bool	-- ast		after parsing
  ,sTraceFns   :: Bool	-- ast		after tracing transform (fns)
  ,sIBound     :: Bool	-- aux tree     after ast annotation

--v pretty-printing flags
  ,sShowWidth  :: Int   -- width for showing intermediate program
  ,sShowIndent :: Int   -- indentation for nesting shown intermediate program
  ,sShowQualified :: Bool -- show qualified ids as far as possible
  
{-
--v hpc flags
  ,sHpcTransFile :: String
  ,sHpcProgName :: String -- from which .pix and .tix names derived
  ,sHpcMixFile :: String
  ,sHpcHierModName :: String
  ,sHpcLevel :: Int
  ,sHpcBoolCover :: Bool
-}
  }
  deriving Show



{- If first argument is True, then print second and third with formatting -}
pF :: Bool -> [Char] -> [Char] -> IO ()
pF flag title text =
  if flag 
    then hPutStr stderr ( "======\t"++title++":\n"++text++"\n") 
    else return ()

{- ---------------------------------------------------------------------------
All the following functions obtain information from the argument list of the
compiler to set flags appropriately.
-}

{-
The main function for processing the argument list.
Aborts with error, if the required filenames are not in argument list.
(But no further error checking)
-}

processArgs :: [String] -> Flags

processArgs xs = flags

 where
{-
 (rootdir,filename) = fixRootDir isUnix sourcefile0
 isUnix = sUnix flags


 (realfile0,sourcefile0) =
   case getFiles xs of
     [sourcefile] -> (sourcefile,sourcefile)
     [realfile,sourcefile] -> (realfile,sourcefile)
     _ -> error ("\nUsage: hat-trans file.[l]hs\n" ++
       	  	 "       hat-trans tmpfile.[l]hs origfile.[l]hs\n")
-}

 flags = FF
  { 
{-
    sRealFile   = realfile0
  , sSourceFile = sourcefile0

    sHatAuxFile = fixHatAuxFile isUnix rootdir filename
  , sHatTransFile = fixHatTransFile isUnix rootdir filename
  , sHatFileBase  = fixHatFileBase isUnix rootdir filename

    sIncludes = getIncludes xs++[rootdir]
  , sPreludes = getPreludes xs

  , sSrcDir   = fixHatTransDir isUnix rootdir
-}

    sUnix = fElem True  "unix" xs          	
  -- ^ Use unix file names
  , sUnlit = fElem False "unlit" xs         	
  -- ^ Unliterate the source code
  , sPrelude = fElem False "prelude" xs		
  -- Keep prelude definitions in interface file

  , sDbgTrusted = fElem False "trusted" xs    -- "trusted" module (don't trace)

  , sUnderscore = fElem True "underscore" xs 
  -- ^ Enable H'98 underscore-is-lower-case

  , sLex = fElem False "lex" xs         -- show lexical input
  , sParse  = fElem False "parse" xs    -- show syntax tree  after  parser
  , sTraceFns = fElem False "tracefns" xs  -- ast after transforming functions
  , sIBound = fElem False "ibound" xs   -- aux tree after ast annotation

  , sShowWidth = cardFlag 80 "showwidth=" xs  -- set width for showing 
                                              -- intermediate program
  , sShowIndent = cardFlag 2 "showindent=" xs -- set indentation for nesting
  , sShowQualified = fElem True "showqualified" xs  
  -- ^ show qualified ids as far as possible
{-
  , sHpcTransFile = stringFlag "hpctrans.out.hs" "transfile=" xs
  , sHpcProgName = stringFlag "a.out" "progname=" xs
  , sHpcMixFile = stringFlag (sourcefile0++".mix") "mixfile=" xs
  , sHpcHierModName = stringFlag "Main" "hiermodname=" xs
  , sHpcLevel = cardFlag 3 "level=" xs
  , sHpcBoolCover = fElem True "boolcover" xs
-}
  }
   
{- obtain list of filenames from argument list -}
getFiles :: [String] -> [String]
getFiles = filter (\xs -> case xs of ('-':_) -> False ; _ -> True)


{- obtain list of include paths from argument list -}
getIncludes :: [String] -> [String]
getIncludes = map (drop (2::Int)) . 
              filter (\xs -> case xs of ('-':'I':_) -> True  
                                        ('-':'i':_) -> True  
                                        _           -> False)

{- obtain list of prelude paths from argument list -}
getPreludes :: [String] -> [String]
getPreludes = map (drop (2::Int)) . 
              filter (\xs -> case xs of ('-':'P':_) -> True ; _ -> False)


{-
Returns if given option is set or not in argument list.
If it is neither set nor unset, then default value (first arg.) is returned.
-}
fElem :: Bool -> [Char] -> [String] -> Bool
fElem def f flags = if ('-':f) `elem` flags then True
                    else if ('-':'n':'o':f) `elem` flags then False
                    else def


{-
Returns the value of an option with a numerical (cardinal) value.
If the option is not given, then the default value (first arg.) is returned.
Ignores syntactically incorrect options.
-}
cardFlag :: Int -> [Char] -> [String] -> Int
cardFlag def f flags = if null settings then def else read (last settings)
  where
  settings = filter (all isDigit) . map (drop (length f + 1)) . 
             filter (isPrefixOf ('-':f)) $ flags



{-
Returns the value of a "-something=" option with a string value.
If the option is not given, then the default value (first arg.) is returned.
-}
stringFlag :: String -> String -> [String] -> String
stringFlag def f flags = if null settings then def else last settings
  where
  settings = map (drop (length f + 1)) . 
             filter (isPrefixOf ('-':f)) $ flags


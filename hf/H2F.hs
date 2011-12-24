----------------------------------------------------------------------------
-- Main module for h2f: a basic translator from a Haskell subset to F-lite
-- Colin Runciman, May 2010.
----------------------------------------------------------------------------

module Main where

import IO
import System
import Directory(getCurrentDirectory)
import Error
import Syntax
import Extra (mixSpace,noPos,strPos,showErr)
import Flags (Flags,processArgs,pF,sUnderscore,sRealFile,sSourceFile,sUnlit
             ,sLex,sParse
             ,sHpcProgName,sHpcTransFile,sHpcHierModName)
import PrettySyntax (prettyPrintTokenId,ppModule)
import TokenId (TokenId(..)
               ,mkQualifiedTokenId,mkUnqualifiedTokenId
               ,tTrue,tFalse, t_Tuple)
import Lex ()  -- need show
import Unlit (unlit)
import Lexical (lexical)
import Parse (parseProg)
import ParseCore(parseit)
import Translate(translate)

-- some miscellaneous settings
primFlags :: (Bool, Bool, Bool)
primFlags = (False   -- bool is not the same as Word
	    ,False   -- && || not is not primitive
	    ,False   -- translate into prim only when strict
	    )

-- some nicer error handling
catchError :: Either b a -> String -> (b->String) -> IO a
catchError comp errmsg showErrors = do
    case comp of
        Left errs -> do pF True errmsg (showErrors errs)
                        exit
        Right a   -> return a

-- for Hugs, which cannot read commandline args using System.getArgs:
gmain :: String -> IO t
gmain cml = main' (words cml)

-- for all other compilers:
main :: IO ()
main = do
  args <- getArgs
  main' args

main' :: [String] -> IO t
main' args = do
  let flags = processArgs args
  source <- getContents
  lexdata	-- :: [PosToken]
           <- return (lexical (sUnderscore flags) (sSourceFile flags)
                              (if sUnlit flags 
                                then unlit (sSourceFile flags) source 
                                else source))
  {-
  pF (sLex flags) "Lexical" 
       (mixSpace (map (\ (p,l,_,_) -> strPos p ++ ':':show l) lexdata))
  -}


  {- parse source code -}
  parsedPrg	-- :: Module TokenId
            <- catchError (parseit parseProg lexdata)
                          ("In file: "++sSourceFile flags) showErr
  {-
  pF (sParse flags) "Parse" (prettyPrintTokenId flags ppModule parsedPrg)
  -} 

  putStr (prettyPrintTokenId flags ppModule (translate flags parsedPrg))
        
  exitWith (ExitSuccess)



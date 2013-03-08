-------------------------------------------------------------------------
-- Main module for hf: a basic translator from a Haskell subset to F-lite
-- Colin Runciman, May 2010.
-------------------------------------------------------------------------

module Main where

import System.Directory(getCurrentDirectory)
import System.Environment
import System.Exit
import Error
import Control.Exception
import Syntax
import Extra (mixSpace,noPos,strPos,showErr)
import Flags (Flags,processArgs,pF,sUnderscore,{-sRealFile,sSourceFile,-}sUnlit
             ,sLex,sParse)
import PrettySyntax (prettyPrintTokenId,ppDecl)
import TokenId (TokenId(..)
               ,mkQualifiedTokenId,mkUnqualifiedTokenId
               ,tTrue,tFalse, t_Tuple)
import Lex ()  -- need show
import Unlit (unlit)
import Lexical (lexical)
import Parse (parseProg)
import ParseCore(parseit)
import Data.List (intersperse)
import Translate(declTrans,mergeDeclFuns,infixPatsToFuns)

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
  lexdata       -- :: [PosToken]
           <- return (lexical (sUnderscore flags) "stdin" source)
{-
                              (if sUnlit flags
                                then unlit (sSourceFile flags) source
                                else source))
-}
  {-
  pF (sLex flags) "Lexical"
       (mixSpace (map (\ (p,l,_,_) -> strPos p ++ ':':show l) lexdata))
  -}


  {- parse source code -}
  parsedPrg     -- :: Module TokenId
            <- catchError (parseit parseProg lexdata)
                          ("") showErr
  {-
  pF (sParse flags) "Parse" (prettyPrintTokenId flags ppModule parsedPrg)
  -}

  transModIO flags parsedPrg

  exitWith (ExitSuccess)

  {-
  putStr (prettyPrintTokenId flags ppModule (translate flags parsedPrg))
  -}

transModIO :: Flags -> Module TokenId -> IO ()
transModIO flags (Module pos modid exps imps fixs (DeclsParse decls)) =
  mapM_ (transDeclIO flags) $
  intersperse Nothing $ map Just $ filter keep $
  mergeDeclFuns $ infixPatsToFuns decls
  where
  keep (DeclFun _ _ _) = True
  keep other           = False

{-transDecls [] = return()
  Module pos modid exps imps fixs decls'
  where
  decls' = declsTrans True decls


declsTrans :: Bool -> Trans (Decls TokenId)
declsTrans top (DeclsParse ds) =
  DeclsParse ds'
  where
  ds' = concatMap (declTrans top)
                  (mergeDeclFuns (infixPatsToFuns ds))
-}

transDeclIO :: Flags -> Maybe (Decl TokenId) -> IO ()
transDeclIO flags (Just d) =
  do
    trans <- try (return $! buffer $ prettyPrintTokenId flags ppDecl d')
    ( case trans of
      Left (ErrorCall s) -> putStrLn $ "-- hf could not translate " ++
                              nameOf d ++ " (" ++ s ++ ")"
      Right t            -> putStr t )
  where
  d' = declTrans True d
  buffer s = foldl const s s
  nameOf (DeclFun _ id _) = show id
transDeclIO _ Nothing =
  putStrLn ""

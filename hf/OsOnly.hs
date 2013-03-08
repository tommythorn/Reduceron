-- operating specific processing of filenames and paths
module OsOnly
  (isPrelude
  , fixImportNames, fixRootDir, fixDependFile, fixTypeFile, fixObjectFile
  , fixHatAuxFile,fixHatTransDir,fixHatTransFile,fixHatFileBase
  , hierarchical
  ) where

import Data.Char (isUpper)

fixDependFile :: Bool -> String -> String -> String
fixHatAuxFile :: Bool -> String -> String -> String
fixHatFileBase :: t -> [a] -> [a] -> [a]
fixHatTransDir :: t -> String -> String
fixHatTransFile :: Bool -> String -> String -> String
fixObjectFile :: Bool -> String -> String -> String
fixTypeFile :: Bool -> String -> String -> String
strip :: (Num t, Eq t) => t -> String -> String

isPrelude :: String -> Bool
isPrelude str = {-take (7::Int)-} str == "Prelude"

-- from complete filename determine path and pure filename without extension
fixRootDir :: Bool -> String -> (String,String)
fixRootDir isUnix s =
 let rs = reverse s
 in
  if isUnix
  then
    case span (/='/') (stripUnix rs) of
      (rf,rr) -> (reverse rr,reverse rf)
  else
    case span (/='.') rs of
      (rf,rr) -> (reverse (stripRiscos rr),reverse rf)
 where
   stripUnix ('s':'h':'l':'.':r) = r
   stripUnix ('s':'h':    '.':r) = r
   stripUnix                  r  = r

   stripRiscos ('.':'s':'h':'l':rr) = rr
   stripRiscos ('.':'s':'h':    rr) = rr
   stripRiscos                  rr  = rr

fixImportNames :: Bool -> String -> String -> [String] -> [String]
fixImportNames isUnix suffix file rootdirs =
  map (\dir-> fixDir isUnix dir ++ (fixFile isUnix file suffix)) rootdirs


-- prepare path so that it can be concatenated with filename
fixDir :: Bool -> String -> String
fixDir isUnix dir
  | isUnix    = case (dir,last dir) of
                    ("",_)  -> ""
                    (_,'/') -> dir
                    (_,_)   -> dir ++ "/"
  | otherwise = dir

fixTypeFile   isUnix rootdir s = rootdir ++ fixFile isUnix s "hi"
fixObjectFile isUnix rootdir s = rootdir ++ fixFile isUnix s "hc"
fixDependFile isUnix rootdir s = rootdir ++ fixFile isUnix s "dep"
fixHatAuxFile isUnix rootdir s = rootdir ++ fixFile isUnix s "hx"
fixHatFileBase _isUnix rootdir s = rootdir ++ s

fixHatTransDir _isUnix rootdir =
  if null rootdir then "Hat"
  else if hierarchical rootdir then "Hat/"++init rootdir
       else rootdir++"Hat"

fixHatTransFile isUnix rootdir s =
  fixHatTransDir isUnix rootdir ++"/"++ fixFile isUnix s "hs"

-- add extension to file
fixFile :: Bool -> String -> String -> String
fixFile isUnix file suf =
{-
  let file =  if isPrelude s
              then case drop (7::Int) s of [] -> s ; r  -> r
              else s
  in
-}
    if isUnix
      then toUnixPath file ++ '.':suf
      else suf ++ '.':maxTen file

toUnixPath :: String -> String
toUnixPath = map (\c-> if (c=='.') then '/' else c)

{- Does a directory name look like a hierarchical module namespace? -}
hierarchical :: String -> Bool
hierarchical dir =
    let (a,b) = break (=='/') dir in
    case b of
      "" -> True
      _  -> case a of
              ""    -> hierarchical (tail b)
              "."   -> False
              ".."  -> False
              (x:_) -> isUpper x && hierarchical (tail b)

-- obscure filename compression needed only for RiscOs:

maxTen :: String -> String
maxTen file = let tolong =  length file - 10
              in if tolong <= 0 then file
                 else take (10::Int) (strip tolong file)

strip 0 xs = xs
strip _n [] = []
strip n (x:xs) = if isVowel x then strip (n-1) xs else x: strip n xs

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel 'y' = True
isVowel '\xe1' = True   -- aa
isVowel '\xe0' = True   -- ae
isVowel '\xf0' = True   -- oe
isVowel 'A' = True
isVowel 'E' = True
isVowel 'I' = True
isVowel 'O' = True
isVowel 'U' = True
isVowel 'Y' = True
isVowel '\xc5' = True   -- AA
isVowel '\xc4' = True   -- AE
isVowel '\xd4' = True   -- OE
isVowel _   = False

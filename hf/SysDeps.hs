{-# OPTIONS -fno-warn-deprecations #-}
module SysDeps (
   module PackedString, trace, isAlphaNum
) where

#if __GLASGOW_HASKELL__ >= 502
import Data.PackedString as PackedString
#else
import PackedString
#endif

#if defined(__NHC__) || defined(__HBC__)
import NonStdTrace (trace)
#elif __GLASGOW_HASKELL__ >= 502
import Debug.Trace (trace)
#else
import IOExts      (trace)
#endif

#if defined(__HASKELL98__)
import Char        (isAlphaNum)
#else
import Char        (isAlphanum)

isAlphaNum :: Char -> Bool
isAlphaNum = isAlphanum
#endif

{-# OPTIONS -fno-warn-deprecations #-}
module SysDeps (trace, isAlphaNum) where

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
import Data.Char   (isAlphaNum)
#endif

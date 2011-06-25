module IOBuffering where

import IO
  ( hSetBuffering
  , stdout
  , BufferMode(..)
  )

noBuffering :: IO ()
noBuffering = hSetBuffering stdout NoBuffering

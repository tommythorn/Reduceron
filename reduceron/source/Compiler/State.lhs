> module Compiler.State where

A simple state monad.  No frills.

> newtype State s a  =  S { runState :: s -> (s, a) }

> instance Monad (State s) where
>   return a         =  S (\s -> (s, a))
>   m >>= f          =  S (\s -> case runState m s of
>                                  (s', a) -> runState (f a) s')

> get                :: State s s
> get                =  S (\s -> (s, s))

> put                :: s -> State s ()
> put s              =  S (\_ -> (s, ()))

> modify             :: (s -> s) -> State s ()
> modify f           =  get >>= put . f

A common usage: fresh name creation.

> type Fresh a       =  State Int a

> fresh              :: Fresh Int
> fresh              =  do n <- get ; put (n+1) ; return n

> runFresh           :: Fresh a -> a
> runFresh m         =  snd (runState m 0)

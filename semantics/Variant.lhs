A crazy trivial version extractor for up to 10 (0 .. 9) versions.

> import Data.Char
> import System.Environment

> main = do
>   args <- getArgs
>   case args of
>     [[v], fileName] | isDigit v
>         -> filterFile v fileName
>     _   -> putStrLn "Usage: version n file"

> filterFile v fn = do
>   s <- readFile fn
>   let s' = unlines $ map (ver v) $ lines s
>   putStr s'

> ver v s = case s of
>   '>':' ':r                             -> r
>   a:      '>':' ':r | a == v            -> r
>   a:'-':  '>':' ':r | a <= v            -> r
>   '-'  :b:'>':' ':r | v <= b            -> r
>   a:'-':b:'>':' ':r | v `elem` [a .. b] -> r
>   s                                 -> comment s
>   where comment "" = ""; comment s = "-- " ++ s

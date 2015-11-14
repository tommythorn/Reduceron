A crazy trivial version extractor for up to 10 (0 .. 9) versions.  It
extracts a version from a versioned literal haskell into "normal"
literal haskell.

> import Data.Char
> import Data.List
> import System.Environment

> main = do
>   args <- getArgs
>   case args of
>     [[v], fileName] | isDigit v
>         -> filterFile v fileName
>     _   -> putStrLn "Usage: version n file"

> filterFile v fn = do
>   s <- readFile fn
>   let classified = [ver v l | l <- lines s]
>       grouped    = map (map snd) $ groupBy eqFst classified
>       spaced     = [s | g <- grouped, s <- g ++ [""]]
>   putStr $ unlines spaced
> eqFst (a, _) (b, _ )= a == b

> ver :: Char -> String -> (Bool, String)
> ver v s = case s of
>   '>':' ':r                             -> (True,  "> " ++ r)
>   a:      '>':' ':r | a == v             -> (True,  "> " ++ r)
>   a:'-':  '>':' ':r | a <= v             -> (True,  "> " ++ r)
>   '-'  :b:'>':' ':r | v <= b             -> (True,  "> " ++ r)
>   a:'-':b:'>':' ':r | v `elem` [a .. b] -> (True,  "> " ++ r)
>   r                                     -> (False, r)

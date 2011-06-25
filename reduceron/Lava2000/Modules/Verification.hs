module Verification
  ( Option(..)
  , verify
  , verifyWith
  , ProofResult(..)
  , verifyDir
  , checkVerifyDir
  )
 where

import Signal
import Netlist
import Generic
import Sequent
import Property
import Error
import LavaDir

import List
  ( intersperse
  , isPrefixOf
  , nub
  )

import IO
  ( openFile
  , IOMode(..)
  , hPutStr
  , hClose
  )

import IOBuffering
  ( noBuffering
  )

import Data.IORef

import System
  ( system
  , ExitCode(..)
  )

verifyDir :: FilePath
verifyDir = "Verify"

----------------------------------------------------------------
-- Options

data Option
  = Name String
  | ShowTime
  | Sat Int
  | NoBacktracking
  | Depth Int
  | Increasing
  | RestrictStates
 deriving (Eq, Show)

defaultOptions :: [Option]
defaultOptions =
  [Increasing]

defaultValues :: [Option]
defaultValues =
  [Name "circuit", Sat 1, Depth 1]

sameOption :: Option -> Option -> Bool
sameOption opt1 opt2 = tag opt1 == tag opt2
 where
  tag = head . words . show

searchOptVal :: Read a => [Option] -> (a -> Option) -> a
searchOptVal options what = search (options ++ defaultValues)
 where
  opt' = what undefined

  search [] = wrong Error.Internal_OptionNotFound
  search (opt:opts)
    | sameOption opt opt' = read (words (show opt) !! 1)
    | otherwise           = search opts

searchOptBool :: [Option] -> Option -> Bool
searchOptBool options what = what `elem` options

----------------------------------------------------------------
-- verify

verify :: Checkable a => a -> IO ProofResult
verify a = verifyWith defaultOptions a

verifyWith :: Checkable a => [Option] -> a -> IO ProofResult
verifyWith options a =
  do checkVerifyDir
     noBuffering

     (props,model) <- properties a
     (states,pins) <- writeDefinitions defsFile props
     res <- proveAll defsFile states pins options
     if res == Falsifiable
       then displayModel outFile model
       else return ()
     return res
 where
  defsFile = verifyDir ++ "/" ++ nameOpt ++ ".defs"
  outFile  = verifyDir ++ "/prover.out"
  nameOpt  = searchOptVal options Name

----------------------------------------------------------------
-- proving

proveAll :: FilePath -> [State] -> Int -> [Option] -> IO ProofResult
proveAll defsFile states quests options =
  case actions of
    [[]] ->
      do putStrLn "Nothing to prove! ... Valid."
         return Valid

    [[(labels, ini, assumps, obligs)]] ->
      do proveUnit labels ini assumps obligs

    _ ->
      do tryAll actions
 where
  depths
    | incrOpt && not (null states) = [depthOpt ..]
    | otherwise                    = [depthOpt]

  steps d
    | null states = [ ([],                  True,  [],     [1]) ]
    | otherwise   = [ (["base " ++ show d], True,  [],     [1..d])
                    , (["step " ++ show d], False, [1..d], [d+1])
                    ]

  pins (labels, ini, assumps, obligs) n
    | n == 1    = [ (labels, ini, map (pin 1) assumps, map (pin 1) obligs) ]
    | otherwise = [ ( labels ++ ["pin " ++ show p]
                    , ini
                    , [ pin p ass | ass <- assumps, p <- [1..n] ]
                      ++ [ pin p' obl | obl <- obligs, p' <- [1..p-1] ]
                    , [ pin p obl | obl <- obligs ]
                    )
                  | p <- [1..n]
                  ]
   where
    pin p a = (a, p)

  actions =
    [ concatMap (`pins` quests) (steps d) | d <- depths ]

  nameOpt  = searchOptVal  options Name
  depthOpt = searchOptVal  options Depth
  incrOpt  = searchOptBool options Increasing
  restrOpt = searchOptBool options RestrictStates

  tryAll [] =
    do result Indeterminate

  tryAll ([] : rest) =
    do result Valid

  tryAll (((labels, ini, assumps, obligs) : tries) : rest) =
    do r <-  proveUnit labels ini assumps obligs
       case r of
         Valid                 -> tryAll (tries : rest)
         Falsifiable | not ini -> tryAll rest
                     | ini     -> result Falsifiable
         Indeterminate         -> result Indeterminate

  result r =
    do putStrLn "--"
       putStrLn ("Result: " ++ show r ++ ".")
       return r

  proveUnit labels ini assumps obligs =
    do proveFile mainFile create labels options
   where
    create =
      do writeHeader
         sequence
              [ instantiateFile defsFile mainFile t
              | t <- times
              ]
         appendAssumptions
            $ [ quest ass
              | ass <- assumps
              ]
           ++ [ "~" ++ init t
              | t <- tail times
              ]
           ++ ( if ini then [init 1]
                       else
                if restrOpt then restrictStates
                            else []
              )
         appendObligations
            $ [ quest obl
              | obl <- obligs
              ]
         appendFooter

    mainFile =
      verifyDir ++ "/" ++ clean (nameOpt ++ ".prov" ++ concatMap ("." ++) labels)

    clean (' ':s) = '_':clean s
    clean (c:s)   = c:clean s
    clean []      = []

    times =
      nub [ t | (t, p) <- assumps ++ obligs ]

    init t =
      "init_t" ++ show t

    quest (t, p) =
      "quest_t" ++ show t ++ "_n" ++ show p

    writeHeader = writeFile mainFile $ unlines $
      [ "/* Generated by Lava 2000 */"
      , "/* options: " ++ show options ++ " */"
      , ""
      , "AND("
      , ""
      ]

    appendAssumptions []  = return ()
    appendAssumptions ass = appendFile mainFile $ unlines $
      [ ""
      , "/* assumptions */"
      , ""
      ] ++ map (++ ",") ass

    appendObligations obs = appendFile mainFile $ unlines $
      [ ""
      , "big_question <-> AND("
      , "/* proof obligations */"
      , ""
      ] ++ map (++ ",") obs

    appendFooter = appendFile mainFile $ unlines $
      [ ""
      , "/* the big question */"
      , "TRUE)) -> big_question"
      , ""
      ]

    restrictStates =
      [ notEqual (t-1) (t'-1) states | t <- times, t' <- times, t' > t ]

    notEqual t t' states =
      "OR(" ++ concat (intersperse ", " [ "~(" ++ s ++ "_t" ++ show t ++ " "
                                          ++ eq ++ " " ++ s ++ "_t" ++ show t' ++ ")"
                                          | (eq,s) <- states ])
            ++ ")"

----------------------------------------------------------------
-- primitive proving

data ProofResult
  = Valid
  | Falsifiable
  | Indeterminate
 deriving (Eq, Show)

proveFile :: FilePath -> IO () -> [String] -> [Option] -> IO ProofResult
proveFile file before labels options =
  do putStr ( "Proving:"
           ++ concat (intersperse "," (map (" " ++) labels))
           ++ " "
            )
     before
     putStr "... "
     lavadir <- getLavaDir
     x <- system ( lavadir
                ++ "/Scripts/prover.wrapper "
                ++ file
                ++ " "
                ++ opts
                ++ " -model first"
                 )
     let res = case x of
                 ExitSuccess   -> Valid
                 ExitFailure 1 -> Indeterminate
                 ExitFailure _ -> Falsifiable
     putStrLn (show res ++ ".")
     return res
 where
  satOpt    = searchOptVal  options Sat
  noBackOpt = searchOptBool options NoBacktracking
  timeOpt   = searchOptBool options ShowTime
  opts      = (if timeOpt then "-showTime " else "")
           ++ ("-t " ++ show satOpt ++ " ")
           ++ (if noBackOpt then "" else "-b")

----------------------------------------------------------------
-- definitions

writeDefinitions :: FilePath -> [Signal Bool] -> IO ([State],Int)
writeDefinitions file props =
  do han <- openFile file WriteMode
     var <- newIORef 0
     sts <- newIORef []

     let new =
           do n <- readIORef var
              let n' = n+1
              writeIORef var n'
              return (Now ("w" ++ show n'))

         define v s =
           do hPutStr han (def ++ ";\n")
              case s of
                DelayBool x y -> state ("<->",the y)
                DelayInt  x y -> state ("=",the y)
                _             -> return ()
          where
           def =
             case s of
               Bool True  -> prop $ "TRUE"
               Bool False -> prop $ "FALSE"
               Inv x      -> prop $ op1 "~" x
               And xs     -> prop $ opl "AND" "," xs "TRUE"
               Or  xs     -> prop $ opl "OR"  "," xs "FALSE"
               Xor xs     -> prop $ opl "XOR" "," xs "FALSE"

               Int   n    -> arit $ show n
               Neg   x    -> arit $ op1 "-" x
               Div   x y  -> arit $ op2 "/" x y
               Mod   x y  -> arit $ op2 "%" x y
               Plus  xs   -> arit $ opl "" "+" xs "0"
               Times xs   -> arit $ opl "" "*" xs "1"
               Gte   x y  -> prop $ op2 ">=" x y
               Equal xs   -> prop $ equal xs
               If x y z   -> iff x (op2 "=" v y) (op2 "=" v z)

               VarBool s  -> prop $ op0 (Now s)
               VarInt  s  -> arit $ op0 (Now s)

               DelayBool x y -> iff ini (prop (op0 x)) (prop (op0 (pre y)))
               DelayInt  x y -> iff ini (arit (op0 x)) (arit (op0 (pre y)))

           prop form =
             "(" ++ show v ++ " <-> (" ++ form ++ "))"

           arit expr =
             "(" ++ show v ++ " = (" ++ expr ++ "))"

           iff c x y =
             "(" ++ op0 c ++ " -> " ++ x ++ ") & (~"
                 ++ op0 c ++ " -> " ++ y ++ ")"

           state s =
             do xs <- readIORef sts
                writeIORef sts (s:xs)

           op0 v      = show v
           op1 op v   = op ++ show v
           op2 op v w = "(" ++ show v ++ " " ++ op ++ " " ++ show w ++ ")"

           opl fun con []  nul = nul
           opl fun con [x] nul = show x
           opl fun con xs  nul =
             fun ++ "(" ++ concat (intersperse con (map show xs)) ++ ")"

           equal (x:y:xs) =
             (show x ++ " = " ++ show y) ++ " & " ++ equal (y:xs)
           equal _ = "TRUE"

     ~(Compound vs) <- netlistIO new define (struct props)
     let quests = [ Index quest i | i <- [1..] ]
     sequence [ hPutStr han (show q ++ " <-> " ++ show v ++ ";\n")
              | (q, Object v) <- quests `zip` vs
              ]
     hClose han

     states <- readIORef sts
     return (states, length vs)

type State
  = (String, String)

data Timed
  = Now String
  | Pre String

instance Show Timed where
  show (Now a) = a ++ "$now"
  show (Pre a) = a ++ "$pre"

the (Now a) = a
the (Pre a) = a
pre (Now a) = Pre a

ini   = Now "init"
quest = Now "quest"

data Indexed
  = Index Timed Int

instance Show Indexed where
  show (Index a n) = show a ++ "_n" ++ show n

----------------------------------------------------------------
-- models

displayModel :: FilePath -> (Model -> [[String]]) -> IO ()
displayModel file showModel =
  do s <- readFile file
     let (first : model) = lines s
     if "# counter model" `isPrefixOf` first
       then do let inps  = inputs model
                   table = makeTable inps []
                   repr  = showModel table
               putStr (unlines (map showInput repr))
       else return ()
 where
  inputs []     = []
  inputs (l:ls) =
    case words l of
      [v@('i':_), "=", val] -> (v, val) : rest
      [v@('i':_)]           -> (v, "high") : rest
      ['~':v@('i':_)]       -> (v, "low") : rest
      _                     -> rest
   where
    rest = inputs ls

  makeTable [] table =
    table

  makeTable ((v,val):inps) table =
    makeTable inps (extend name (read time) val table)
   where
    n    = length v
    name = take (n - length time - 2) v
    time = reverse . takeWhile isDigit . reverse $ v
    isDigit d = '0' <= d && d <= '9'

  extend name time val [] =
    extend name time val [(name, [])]

  extend name time val (entry@(name',vals):table)
    | name == name' = updated : table
    | otherwise     = entry : extend name time val table
   where
    updated = (name', make (time - 1) vals ++ [val] ++ drop time vals)

    make 0 xs     = []
    make n []     = make n ["?"]
    make n (x:xs) = x : make (n-1) xs

  showInput [x] = x
  showInput xs  = "<" ++ concat (intersperse "," xs) ++ ">"

----------------------------------------------------------------
-- instantiation

instantiateFile :: FilePath -> FilePath -> Int -> IO ()
instantiateFile from to t =
  do appendFile to ("/* time instance " ++ show t ++ " */\n")
     system ( "sed "
           ++ "-e 's/$now/_t" ++ show t     ++ "/g' "
           ++ "-e 's/$pre/_t" ++ show (t-1) ++ "/g' "
           ++ "-e 's/;/,/g' "
           ++ " < " ++ from ++ " >> " ++ to
            )
     return ()

----------------------------------------------------------------
-- helper functions

checkVerifyDir :: IO ()
checkVerifyDir =
  do system ("mkdir -p " ++ verifyDir)
     return ()

----------------------------------------------------------------
-- the end.


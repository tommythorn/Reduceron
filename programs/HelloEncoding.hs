-- Encoding a string as [Char] is very inefficient in Reduceron (2 apps per char).
-- Instead we can encode N chars in an int and up to three ints in an app.
-- Here we just use N=1

data DenseString
     = DS0
     | DS1 Char
     | DS2 Char Char
     | DS3 Char Char Char
     | DSn Char Char DenseString
     deriving Show

decode :: DenseString -> String
decode ds = case ds of
     DS0                        -> []
     DS1 a                      -> [a]
     DS2 a b                    -> [a, b]
     DS3 a b c                  -> [a, b, c]
     DSn a b ds                 -> a : b : decode ds

encode s = case s of
     []                         -> DS0
     [a]                        -> DS1 a
     [a, b]                     -> DS2 a b
     [a, b, c]                  -> DS3 a b c
     a : b : s                  -> DSn a b (encode s)

main = putStrLn $ show $ encode  "At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis ..."


-- Here we just use N=2 (and 128 bit chars as Reduceron currently only have 15-bit ints)

type TwoChar = Int
data DenseString2
     = DSB0
     | DSB1 Char
     | DSB2 Char Char
     | DSB3 Char Char Char
     | DSB4 TwoChar TwoChar
     | DSB5 Char TwoChar TwoChar
     | DSB6 TwoChar TwoChar TwoChar
     | DSBn TwoChar TwoChar DenseString2
     deriving Show

enTwo :: Char -> Char -> Int
enTwo a b = fromEnum a + 128 * fromEnum b
encode2 s = case s of
     []                         -> DSB0
     [a]                        -> DSB1 a
     [a, b]                     -> DSB2 a b
     [a, b, c]                  -> DSB3 a b c
     [a, b, c, d]               -> DSB4 (enTwo a b) (enTwo c d)
     [a, b, c, d, e]            -> DSB5 a (enTwo b c) (enTwo d e)
     [a, b, c, d, e, f]         -> DSB6 (enTwo a b) (enTwo c d) (enTwo e f)
     a : b : c : d : s          -> DSBn (enTwo a b) (enTwo c d) (encode2 s)

deTwo :: Int -> String -> String
deTwo ab k = toEnum (ab `mod` 128) : toEnum (ab `div` 128) : k
decode2 :: DenseString2 -> String
decode2 ds = case ds of
     DSB0                        -> []
     DSB1 a                      -> [a]
     DSB2 a b                    -> [a, b]
     DSB3 a b c                  -> [a, b, c]
     DSB4 ab cd                  -> deTwo ab $ deTwo cd []
     DSB5 a bc de                -> a : deTwo bc (deTwo de [])
     DSB6 ab cd de               -> deTwo ab $ deTwo cd $ deTwo de []
     DSBn ab cd ds               -> deTwo ab $ deTwo cd $ decode2 ds

main2 = putStrLn $ show $
  encode2 "At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis ..."

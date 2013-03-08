duplicates :: Eq a => [a] -> [a]
duplicates []      =  []
duplicates (x:xs)  =
  if not (contains d x) && contains xs x then x:d else d
  where
  d  =  duplicates xs

prefixes :: [a] -> [[a]]
prefixes []      =  [[]]
prefixes (x:xs)  =  [] : map (x:) (prefixes xs)

suffixes :: [a] -> [[a]]
suffixes []      =  [[]]
suffixes (x:xs)  =  (x:xs) : suffixes xs

perms :: [a] -> [[a]]
perms []      =  [[]]
perms xs      =  [x:p | (x,xs') <- picks xs, p <- perms xs']

picks :: [a] -> [(a,[a])]
picks []      =  []
picks (x:xs)  =  (x,xs) : [(x',x:xs') | (x',xs') <- picks xs]

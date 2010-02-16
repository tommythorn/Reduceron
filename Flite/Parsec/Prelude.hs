module Flite.Parsec.Prelude(
		prelude,
		supplyPrelude
	) where
	import Flite.Descend
	import Flite.Pretty
	import Flite.Traversals
	
	import Flite.Syntax
	
	prelude :: [Decl]
	prelude = [ Func "(.)" [Var "f", Var "g", Var "x"] (App (Var "f") [App (Var "g") [App (Var "x") []]])
			  , Func "($)" [Var "f", Var "x"] (App (Var "f") [App (Var "x") []])
			  , Func "scNoDrive" [Var "x"] (Var "x") ]
	
	supplyPrelude :: Prog -> Prog
	supplyPrelude p = foldr addFunc p prelude
		where
			addFunc d@(Func f _ _) p | f `elem` map funcName p = p
									 | f `elem` concatMap (calls . funcRhs) p = d:p
									 | f `elem` concatMap (freeVars . funcRhs) p = d:p
									 | otherwise = p

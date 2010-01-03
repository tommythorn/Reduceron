module Flite.Parsec.Parse where
	import qualified Flite.Parsec.Prelude as Prelude
	import Flite.Syntax
	import Flite.Pretty

	import Control.Applicative
	import Control.Monad
	import Data.Char
	import Text.ParserCombinators.Parsec hiding (many, option, (<|>))
	import Text.ParserCombinators.Parsec.Expr
	import Text.ParserCombinators.Parsec.Language
	import qualified Text.ParserCombinators.Parsec.Token as T
	
	flite = T.makeTokenParser $ emptyDef
		{ commentLine 		= "--"
		, nestedComments 	= False
		, identStart		= letter
		, identLetter		= alphaNum
		, opStart			= opLetter haskellStyle
		, opLetter			= oneOf "<=>-+/.$"
		, reservedNames		= ["case", "of", "let", "in", "if", "then", "else"]
		, caseSensitive		= True
		}
	
	identifier = T.identifier flite
	reservedOp = T.reservedOp flite
	reserved = T.reserved flite
	natural = T.natural flite
	parens = T.parens flite
	semi = T.semi flite
	braces = T.braces flite
	symbol = T.symbol flite
	operator = T.operator flite
	charLiteral = T.charLiteral flite
	stringLiteral = T.stringLiteral flite
	
	instance Applicative (GenParser s a) where
	    pure  = return
	    (<*>) = ap
	
	instance Alternative (GenParser s a) where
	    empty = mzero
	    (<|>) = mplus
	
	prog :: Parser Prog
	prog = block defn
	
	block :: Parser a -> Parser [a]
	block p = braces (p `sepEndBy` semi) <?> "block"
	
	primitives = ["(+)", "(-)", "(==)", "(/=)", "(<=)", "emit", "emitInt"]
	
	-- | Build an application out of an infix operation
	infixApp t x y = App t [x, y]
	
	-- | Detect if an identifier if variable or constructor
	consOrVar n = if isLower (head n) then Var n else Con n
	
	-- | Capture \`InfixFunctions\`
	infixName = Infix ((symbol "`" *> identifier <* symbol "`") >>= return . infixApp . consOrVar ) AssocLeft
	
	-- | Build primitive infix operators
	binary op assoc = Infix (reservedOp op >> return (infixApp (Fun $ "(" ++ op ++ ")"))) assoc
	
	opTable = [   [infixName, binary "." AssocRight]
				, [binary "+" AssocLeft, binary "-" AssocLeft]
				, [binary "==" AssocNone, binary "/=" AssocNone, binary "<=" AssocNone]
				, [binary "$" AssocRight] ]
	
	prim :: Parser Id
	prim = try $ do
		v <- identifier
		 <|> pure (++) <*> symbol "(" <*> (pure (++) <*> operator <*> symbol ")")
		if v `elem` primitives
			then return v
			else unexpected (show v) <?> "primitive"
	
	var :: Parser Id
	var = try $ do
		v <- identifier
		if isLower (head v)
			then return v
			else unexpected ("constructor " ++ show v) <?> "variable"
	
	con :: Parser Id
	con = try $ do
		c <- identifier
		if isUpper (head c)
			then return c
			else unexpected ("variable " ++ show c) <?> "constructor"
	
	defn :: Parser Decl
	defn = pure Func <*> var <*> many pat <*> (reservedOp "=" *> expr) <?> "definition"
		
	pat :: Parser Exp
	pat = pure Var <*> var
		<|> pure App <*> (pure Con <*> con) <*> pure []
		<|> parens pat'
		<?> "pattern"
	
	pat' :: Parser Exp
	pat' = pure Var <*> var
		<|> pure App <*> (pure Con <*> con) <*> many pat
	
	expr :: Parser Exp
	expr = buildExpressionParser opTable (pure App <*> expr' <*> many expr')
	
	expr' :: Parser Exp
	expr' = pure Lam <*> ((symbol "\\" <?> "lambda abstraction") *> many var) <*> (reservedOp "->" *> expr)
		<|> pure Case <*> (reserved "case" *> expr) <*> (reserved "of" *> block alt)
		<|> pure Let <*> (reserved "let" *> block bind) <*> (reserved "in" *> expr)
		<|> pure ifthenelse <*> (reserved "if" *> expr) <*> (reserved "then" *> expr) <*> (reserved "else" *> expr)
		<|> pure Fun <*> prim
		<|> pure Var <*> var
		<|> pure Con <*> con
		<|> pure Int <*> (pure fromInteger <*> natural)
		<|> pure (Int . ord) <*> charLiteral
		<|> pure stringExp <*> stringLiteral
		<|> parens expr
	
	ifthenelse :: Exp -> Exp -> Exp -> Exp
	ifthenelse x y z = Case x [(App (Con "True") [], y), (App (Con "False") [], z)]
	
	stringExp :: String -> Exp
	stringExp [] = App (Con "Nil") []
	stringExp (x:xs) = App (Con "Cons") [Int . ord $ x, stringExp xs]
	
	alt :: Parser Alt
	alt = pure (,) <*> pat' <*> (reservedOp "->" *> expr)
	
	bind :: Parser Binding
	bind = pure (,) <*> var <*> (reservedOp "=" *> expr)
	
	parseProgFile :: SourceName -> IO Prog
	parseProgFile f = parseFromFile prog f >>= \result -> case result of
						Left e	-> error . show $ e
						Right p	-> return . Prelude.supplyPrelude $ p

	
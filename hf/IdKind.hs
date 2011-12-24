{- ---------------------------------------------------------------------------
Defines data type IdKind which tells of which kind an identifier is, 
e.g. a module identifier or a class identifier
-}

module IdKind(IdKind(..)) where

data IdKind = Var
            | Arg
            | Method
            | TVar
            | Con
            | TCon
            | TSyn
            | TClass
            | TC
            | Modid
            | MethodInstance
            | MethodDefault
	    | Field

ordIdKind :: IdKind -> Int
ordIdKind  Var    =  1
ordIdKind  Arg    =  1
ordIdKind  Method =  1
ordIdKind  TVar   =  2
ordIdKind  Con    =  3
ordIdKind  TCon   =  4
ordIdKind  TSyn   =  4
ordIdKind  TClass =  4
ordIdKind  TC     =  4
ordIdKind  Modid  =  5
ordIdKind  MethodInstance =  6
ordIdKind  MethodDefault  = 7
ordIdKind  Field =   8

instance Eq IdKind where
         a      == b      = ordIdKind a == ordIdKind b

instance Ord IdKind where
         a  <= b = ordIdKind a <= ordIdKind b
         a  <  b = ordIdKind a <  ordIdKind b
         compare a  b = compare (ordIdKind a) (ordIdKind b)


instance Show IdKind where
  showsPrec _d Var    = ("Identifier"++)
  showsPrec _d Arg    = ("Argument"++)
  showsPrec _d Method = ("Method"++)
  showsPrec _d TVar   = ("Typevar"++)
  showsPrec _d Con    = ("Constructor"++)
  showsPrec _d TC     = ("Type constructor/class"++)
  showsPrec _d TCon   = ("Type constructor"++)
  showsPrec _d TSyn   = ("Type synonym"++)
  showsPrec _d TClass = ("Type class"++)
  showsPrec _d Modid  = ("Module identifier"++)
  showsPrec _d MethodDefault  = ("Default method"++)
  showsPrec _d MethodInstance = ("Instance method"++)
  showsPrec _d Field = ("Field"++)


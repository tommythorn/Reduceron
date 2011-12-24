module Lex(Lex(..),LexAnnot(..),TokenId) where

import Extra(strStr,strChr)
import TokenId(TokenId)
import Ratio

data LexAnnot = LexArity     TokenId Int
              | LexPrimitive TokenId (String,String)  -- id (opcode,strictness)

instance Show LexAnnot where
  showsPrec _d (LexArity     fun a)   = showString "{#- ARITY " . shows fun . showString " = " . shows a  . showString "#-}"
  showsPrec _d (LexPrimitive fun s)   = showString "{#- PRIMITIVE ". shows fun . showString " = " . shows s  . showString "#-}"

data Lex = 
      L_EOF
    | L_ERROR Char     -- illegal character
    | L_LPAR
    | L_RPAR
    | L_LANNOT
    | L_RANNOT
    | L_LCURL | L_LCURL'
    | L_RCURL | L_RCURL'
    | L_LBRACK
    | L_RBRACK
    | L_BACKTICK
    | L_COMMA
    | L_SEMI  | L_SEMI'

-- reserved ops
    | L_DotDot
    | L_ColonColon
    | L_EqualGreater
    | L_Equal
    | L_At
    | L_Lambda
    | L_Pipe
    | L_Tidle
    | L_LessMinus
    | L_MinusGreater
    | L_Underscore

    | L_AVARID TokenId
    | L_AVAROP TokenId
    | L_ACONID TokenId
    | L_ACONOP TokenId

    | L_INTEGER Integer
    | L_RATIONAL Rational
    | L_DOUBLE  Double
    | L_CHAR    Char
    | L_STRING [Char]

-- reserved words
    | L_case
    | L_class
    | L_data
    | L_default
    | L_deriving
    | L_do
    | L_else
    | L_if
    | L_import
    | L_in
    | L_infix
    | L_infixl
    | L_infixr
    | L_instance
    | L_let
    | L_module
    | L_newtype
    | L_of
    | L_then
    | L_type
    | L_where
--    | L_as
--    | L_hiding
--    | L_prefix
--    | L_primitive
--    | L_interface
--    | L_qualified
--    | L_renaming
--    | L_unboxed
--    | L_with

instance Eq Lex where
    L_EOF          == L_EOF         = True
    (L_ERROR _)    == (L_ERROR _)   = True
    L_LPAR         == L_LPAR        = True
    L_RPAR         == L_RPAR        = True
    L_LANNOT       == L_LANNOT      = True
    L_RANNOT       == L_RANNOT      = True
    L_LCURL        == L_LCURL       = True
    L_LCURL'       == L_LCURL'      = True
    L_RCURL        == L_RCURL       = True
    L_RCURL'       == L_RCURL'      = True
    L_LBRACK       == L_LBRACK      = True
    L_RBRACK       == L_RBRACK      = True

    L_BACKTICK     == L_BACKTICK     = True
    L_COMMA        == L_COMMA        = True
    L_SEMI         == L_SEMI         = True
    L_SEMI'        == L_SEMI'        = True
    L_DotDot       == L_DotDot       = True
    L_ColonColon   == L_ColonColon   = True
    L_EqualGreater == L_EqualGreater = True
    L_Equal        == L_Equal        = True
    L_At           == L_At           = True
    L_Lambda       == L_Lambda       = True
    L_Pipe         == L_Pipe         = True
    L_Tidle        == L_Tidle        = True
    L_LessMinus    == L_LessMinus    = True
    L_MinusGreater == L_MinusGreater = True
    L_Underscore   == L_Underscore   = True

    (L_AVARID a) == (L_AVARID b) = a==b
    (L_AVAROP a) == (L_AVAROP b) = a==b
    (L_ACONID a) == (L_ACONID b) = a==b
    (L_ACONOP a) == (L_ACONOP b) = a==b

    (L_INTEGER _) == (L_INTEGER _)    = True
    (L_RATIONAL _) == (L_RATIONAL _)    = True
    (L_DOUBLE _)  == (L_DOUBLE _)  = True
    (L_CHAR _)   == (L_CHAR _)   = True
    (L_STRING _) == (L_STRING _) = True

    L_case      == L_case      = True
    L_class     == L_class     = True
    L_data      == L_data      = True
    L_default   == L_default   = True
    L_deriving  == L_deriving  = True
    L_do        == L_do        = True
    L_else      == L_else      = True
    L_if        == L_if        = True
    L_import    == L_import    = True
    L_in        == L_in        = True
    L_infix     == L_infix     = True
    L_infixl    == L_infixl    = True
    L_infixr    == L_infixr    = True
    L_instance  == L_instance  = True
    L_let       == L_let       = True
    L_module    == L_module    = True
    L_newtype   == L_newtype   = True
    L_of        == L_of        = True
    L_then      == L_then      = True
    L_type      == L_type      = True
    L_where     == L_where     = True
--    L_as        == L_as        = True
--    L_hiding    == L_hiding    = True
--    L_interface == L_interface = True
--    L_prefix    == L_prefix    = True
--    L_primitive == L_primitive = True
--    L_qualified == L_qualified = True
--    L_renaming  == L_renaming  = True
--    L_unboxed   == L_unboxed   = True
--    L_with      == L_with      = True
    _           == _           = False

instance Show Lex where
	-- Note: EOF really means end-of-file, but because error messages
	--   saying "got blah but expected EOF" are less than helpful,
	--   I have changed the string for EOF to indicate the likely cause
	--   of the parse error.
  showsPrec _d (L_EOF)       = showString  "{-end-of-definition-or-EOF-}"
  showsPrec _d (L_ERROR c)   = showString  "{-ERROR " . showChar c . showString "-}"
  showsPrec _d (L_LPAR )     = showString  "("
  showsPrec _d (L_RPAR )     = showString  ")"
  showsPrec _d (L_LANNOT )   = showString  "{-#"
  showsPrec _d (L_RANNOT )   = showString  "#-}"
  showsPrec _d (L_LCURL )    = showString  "{"
  showsPrec _d (L_RCURL )    = showString  "}"
  showsPrec _d (L_LCURL' )   = showString  "{-start-of-group-}"
  showsPrec _d (L_RCURL' )   = showString  "{-end-of-group-}"
  showsPrec _d (L_LBRACK )   = showString  "["
  showsPrec _d (L_RBRACK )   = showString  "]"
  showsPrec _d (L_BACKTICK ) = showString  "`"
  showsPrec _d (L_COMMA )    = showString  ","
  showsPrec _d (L_SEMI )     = showString  ";"
  showsPrec _d (L_SEMI' )    = showString  "{-end-of-definition-}"

  showsPrec _d (L_DotDot)       = showString  ".."
  showsPrec _d (L_ColonColon)   = showString  "::"
  showsPrec _d (L_EqualGreater) = showString  "=>"
  showsPrec _d (L_Equal)        = showString  "="
  showsPrec _d (L_At)           = showString  "@"
  showsPrec _d (L_Lambda)       = showString  "\\"
  showsPrec _d (L_Pipe)         = showString  "|"
  showsPrec _d (L_Tidle)        = showString  "~"
  showsPrec _d (L_LessMinus)    = showString  "<-"
  showsPrec _d (L_MinusGreater) = showString  "->"
  showsPrec _d (L_Underscore)   = showString  "_ " 
  showsPrec _d (L_AVARID s)     = shows s
  showsPrec _d (L_AVAROP s)     = showChar '(' . shows s . showChar ')'
  showsPrec d (L_ACONID s)     = showsPrec d  s
  showsPrec _d (L_ACONOP s)     = showChar '(' . shows s . showChar ')'

  showsPrec _d (L_INTEGER i)  = shows i
  showsPrec _d (L_RATIONAL i)  = shows i
  showsPrec _d (L_DOUBLE f)   = shows f
  showsPrec _d (L_CHAR c)     = showString (strChr c)
  showsPrec _d (L_STRING s)   = showString (strStr s)

  showsPrec _d (L_case )      = showString "_case_"
  showsPrec _d (L_class )     = showString "_class_"
  showsPrec _d (L_data )      = showString "_data_"
  showsPrec _d (L_default )   = showString "_default_"
  showsPrec _d (L_deriving )  = showString "_deriving_"
  showsPrec _d (L_do )        = showString "_do_"
  showsPrec _d (L_else )      = showString "_else_"
  showsPrec _d (L_if )        = showString "_if_"
  showsPrec _d (L_import )    = showString "_import_"
  showsPrec _d (L_in )        = showString "_in_"
  showsPrec _d (L_infix )     = showString "_infix_"
  showsPrec _d (L_infixl )    = showString "_infixl_"
  showsPrec _d (L_infixr )    = showString "_infixr_"
  showsPrec _d (L_instance )  = showString "_instance_"
  showsPrec _d (L_let )       = showString "_let_"
  showsPrec _d (L_module )    = showString "_module_"
  showsPrec _d (L_newtype)    = showString "_newtype_"
  showsPrec _d (L_of )        = showString "_of_"
  showsPrec _d (L_then )      = showString "_then_"
  showsPrec _d (L_type )      = showString "_type_"
  showsPrec _d (L_where )     = showString "_where_"
--  showsPrec _d (L_as )        = showString "_as_"
--  showsPrec _d (L_hiding )    = showString "_hiding_"
--  showsPrec _d (L_interface ) = showString "_interface_"
--  showsPrec _d (L_prefix )    = showString "_prefix_"
--  showsPrec _d (L_primitive)  = showString "_primitive_"
--  showsPrec _d (L_qualified)  = showString "_qualified_"
--  showsPrec _d (L_renaming )  = showString "_renaming_"
--  showsPrec _d (L_unboxed)    = showString "_unboxed_"
--  showsPrec _d (L_with )      = showString "_with_"

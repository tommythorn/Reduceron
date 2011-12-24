module LexStr(tab,lexStr,lexChr) where

import Char

import Error(errorLC)
import LexLow

tab :: Int -> Int
tab l = (m*8+1) where m = (l+7)`div`8

lexStr :: Int->Int->[Char] -> (Int,Int,[Char],[Char])
lexStr r c xs = pS '"' (r,c) r c [] xs

lexChr :: Int->Int->[Char] -> (Int,Int,Char,[Char])
lexChr r c xs = case pS '\'' (r,c) r c [] xs of
                (r',c',[ch],xs') -> (r',c',ch,xs')
                (_,_,[] ,_) -> errorLC r c "Empty character literal."
                (_,_,cs ,_) -> errorLC r c ("Character literal with length>1 '"
                                            ++ cs ++ "'.")

---- local


pS :: Char -> (Int,Int) -> Int -> Int -> [Char] -> [Char]
      -> (Int,Int,[Char],[Char])
pS _d (ro,co) _r _c _a []  = errorLC ro co "Unterminated literal string/character."
pS _d  _roco   r c _a ('\n':_) = errorLC r c ("Newline in literal string/char."
                                           ++"\nUse \\n instead.")
pS _d  _roco   r c _a ('\t':_) = errorLC r c ("Tab in literal string/char."
                                           ++"\nUse \\t instead.")
pS _d  _roco   r c _a ('\f':_) = errorLC r c ("Linefeed in literal string/char."
                                           ++"\nUse \\f instead.")
pS _d  _roco   r c _a ('\v':_) = errorLC r c ("Vertical tab in literal string/char"
                                           ++".\nUse \\v instead.")
pS d  roco   r c a ('\\':'&':xs) = pS d roco r (c+2) a xs
pS d  roco   r c a ('\\':'a':xs) = pS d roco r (c+2) ('\a':a) xs
pS d  roco   r c a ('\\':'b':xs) = pS d roco r (c+2) ('\b':a) xs
pS d  roco   r c a ('\\':'f':xs) = pS d roco r (c+2) ('\f':a) xs
pS d  roco   r c a ('\\':'n':xs) = pS d roco r (c+2) ('\n':a) xs
pS d  roco   r c a ('\\':'r':xs) = pS d roco r (c+2) ('\r':a) xs
pS d  roco   r c a ('\\':'t':xs) = pS d roco r (c+2) ('\t':a) xs
pS d  roco   r c a ('\\':'v':xs) = pS d roco r (c+2) ('\v':a) xs
pS d  roco   r c a ('\\':'\\':xs) = pS d roco r (c+2) ('\\':a) xs
pS d  roco   r c a ('\\':'"':xs) = pS d roco r (c+2) ('"':a) xs
pS d  roco   r c a ('\\':'\'':xs)= pS d roco r (c+2) ('\'':a) xs
pS d  roco   r c a ('\\':'N':'U':'L':xs) = pS d roco r (c+4) ('\NUL':a) xs
pS d  roco   r c a ('\\':'S':'O':'H':xs) = pS d roco r (c+4) ('\SOH':a) xs
pS d  roco   r c a ('\\':'S':'T':'X':xs) = pS d roco r (c+4) ('\STX':a) xs
pS d  roco   r c a ('\\':'E':'T':'X':xs) = pS d roco r (c+4) ('\ETX':a) xs
pS d  roco   r c a ('\\':'E':'O':'T':xs) = pS d roco r (c+4) ('\EOT':a) xs
pS d  roco   r c a ('\\':'E':'N':'Q':xs) = pS d roco r (c+4) ('\ENQ':a) xs
pS d  roco   r c a ('\\':'A':'C':'K':xs) = pS d roco r (c+4) ('\ACK':a) xs
pS d  roco   r c a ('\\':'B':'E':'L':xs) = pS d roco r (c+4) ('\BEL':a) xs
pS d  roco   r c a ('\\':'B':'S':xs)     = pS d roco r (c+3) ('\BS':a) xs
pS d  roco   r c a ('\\':'H':'T':xs)     = pS d roco r (c+3) ('\HT':a) xs
pS d  roco   r c a ('\\':'L':'F':xs)     = pS d roco r (c+3) ('\LF':a) xs
pS d  roco   r c a ('\\':'V':'T':xs)     = pS d roco r (c+3) ('\VT':a) xs
pS d  roco   r c a ('\\':'F':'F':xs)     = pS d roco r (c+3) ('\FF':a) xs
pS d  roco   r c a ('\\':'C':'R':xs)     = pS d roco r (c+3) ('\CR':a) xs
pS d  roco   r c a ('\\':'S':'O':xs)     = pS d roco r (c+3) ('\SO':a) xs
pS d  roco   r c a ('\\':'S':'I':xs)     = pS d roco r (c+3) ('\SI':a) xs
pS d  roco   r c a ('\\':'D':'L':'E':xs) = pS d roco r (c+4) ('\DLE':a) xs
pS d  roco   r c a ('\\':'D':'C':'1':xs) = pS d roco r (c+4) ('\DC1':a) xs
pS d  roco   r c a ('\\':'D':'C':'2':xs) = pS d roco r (c+4) ('\DC2':a) xs
pS d  roco   r c a ('\\':'D':'C':'3':xs) = pS d roco r (c+4) ('\DC3':a) xs
pS d  roco   r c a ('\\':'D':'C':'4':xs) = pS d roco r (c+4) ('\DC4':a) xs
pS d  roco   r c a ('\\':'N':'A':'K':xs) = pS d roco r (c+4) ('\NAK':a) xs
pS d  roco   r c a ('\\':'S':'Y':'N':xs) = pS d roco r (c+4) ('\SYN':a) xs
pS d  roco   r c a ('\\':'E':'T':'B':xs) = pS d roco r (c+4) ('\ETB':a) xs
pS d  roco   r c a ('\\':'C':'A':'N':xs) = pS d roco r (c+4) ('\CAN':a) xs
pS d  roco   r c a ('\\':'E':'M':xs)     = pS d roco r (c+3) ('\EM':a) xs
pS d  roco   r c a ('\\':'S':'U':'B':xs) = pS d roco r (c+4) ('\SUB':a) xs
pS d  roco   r c a ('\\':'E':'S':'C':xs) = pS d roco r (c+4) ('\ESC':a) xs
pS d  roco   r c a ('\\':'F':'S':xs)     = pS d roco r (c+3) ('\FS':a) xs
pS d  roco   r c a ('\\':'G':'S':xs)     = pS d roco r (c+3) ('\GS':a) xs
pS d  roco   r c a ('\\':'R':'S':xs)     = pS d roco r (c+3) ('\RS':a) xs
pS d  roco   r c a ('\\':'U':'S':xs)     = pS d roco r (c+3) ('\US':a) xs
pS d  roco   r c a ('\\':'S':'P':xs)     = pS d roco r (c+3) ('\SP':a) xs
pS d  roco   r c a ('\\':'D':'E':'L':xs) = pS d roco r (c+4) ('\DEL':a) xs
pS d  roco   r c a ('\\':'^':x:xs) =
      if ox >= 64 && ox <96 then     -- Fusk ?
          pS d roco r (c+3) (toEnum (ox-64):a) xs 
      else
          errorLC r c ("Illegal control character '\\^" ++ [x]
                        ++ "' in string or character literal.")
      where ox = fromEnum x
pS d  roco   r c a ('\\':'o':xs) = pS d roco r c' (toEnum (fromInteger n):a) xs'
      where (c',n,xs') = lexInteger  8 (c+2) xs
pS d  roco   r c a ('\\':'x':xs) = pS d roco r c' (toEnum (fromInteger n):a) xs'
      where (c',n,xs') = lexInteger (8+8) (c+2) xs
pS d  roco   r c a ('\\':' ':xs)  = pS d roco r' c' a xs'
      where (r',c',xs') = pW r (c+2) xs
pS d  roco   r c a ('\\':'\t':xs) = pS d roco r' c' a xs'
      where (r',c',xs') = pW r (tab(c+1)) xs
pS d  roco   r _c a ('\\':'\n':xs) = pS d roco r' c' a xs'
      where (r',c',xs') = pW (r+1) 1 xs
pS d  roco   r c a ('\\':x:xs) =
      if isDigit x then
          case lexInteger 10 (c+1) (x:xs) of
		 (c',i,xs') -> pS d roco r c' (toEnum (fromInteger i):a) xs'
      else
          errorLC r c ("Illegal escape character '" ++ x: "'.")
pS d  roco   r c a (x:xs) =
      if d == x then
          (r,c+1,reverse a,xs)
      else
          pS d roco r (c+1) (x:a) xs

pW :: Int -> Int -> [Char] -> (Int,Int,[Char])
pW r c [] =    errorLC r c "End of file in string gap."
pW r c ('\\':xs) = (r,c+1,xs)
pW r c (' ':xs) =  pW r (c+1) xs
pW r c ('\t':xs) = pW r (tab c) xs
pW r _c ('\n':xs) = pW (r+1) 1 xs
pW r _c ('\f':xs) = pW (r+1) 1 xs
pW r _c ('\v':xs) = pW (r+1) 1 xs
pW r c (x:_xs) = errorLC r c ("Illegal character in string gap '" ++ x:"'.")



{
text = DSn 'A' 't' (DSn ' ' 'v' (DSn 'e' 'r' (DSn 'o' ' ' (DSn 'e' 'o' (DSn 's' ' ' (DSn 'e' 't' (DSn ' ' 'a' (DSn 'c' 'c' (DSn 'u' 's' (DSn 'a' 'm' (DSn 'u' 's' (DSn ' ' 'e' (DSn 't' ' ' (DSn 'i' 'u' (DSn 's' 't' (DSn 'o' ' ' (DSn 'o' 'd' (DSn 'i' 'o' (DSn ' ' 'd' (DSn 'i' 'g' (DSn 'n' 'i' (DSn 's' 's' (DSn 'i' 'm' (DSn 'o' 's' (DSn ' ' 'd' (DSn 'u' 'c' (DSn 'i' 'm' (DSn 'u' 's' (DSn ' ' 'q' (DSn 'u' 'i' (DSn ' ' 'b' (DSn 'l' 'a' (DSn 'n' 'd' (DSn 'i' 't' (DSn 'i' 'i' (DSn 's' ' ' (DS3 '.' '.' '.')))))))))))))))))))))))))))))))))))));

decode cons nil ds = case ds of {
     DS0                        -> nil;
     DS1 a                      -> cons a nil;
     DS2 a b                    -> cons a (cons b nil);
     DS3 a b c                  -> cons a (cons b (cons c nil));
     DSn a b ds                 -> cons a (cons b (decode cons nil ds));
};

putChar ch rest = st32 0 ch rest;

main = decode putChar 42 text;
}

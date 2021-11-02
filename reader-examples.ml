# test_string nt_sexpr "#t" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 2; found = ScmBoolean true}
# test_string nt_sexpr "#T" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 2; found = ScmBoolean true}
# test_string nt_sexpr "#f" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 2; found = ScmBoolean false}
# test_string nt_sexpr "#F" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 2; found = ScmBoolean false}
# test_string nt_sexpr "#\\a" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 3; found = ScmChar 'a'}
# test_string nt_sexpr "#\\A" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 3; found = ScmChar 'A'}
# test_string nt_sexpr "\"\"" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 2; found = ScmString ""}
# test_string nt_sexpr "\"moshe!\"" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 8; found = ScmString "moshe!"}
# test_string nt_sexpr "\"moshe!\\n\\t\\r\\f\"" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 16; found = ScmString "moshe!\n\t\r\012"}
# test_string nt_sexpr "\"The letter 'a' can be entered as \\x61;\"" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 40;
 found = ScmString "The letter 'a' can be entered as a"}
# test_string nt_sexpr "\"The letter 'A' can be entered as \\x41;\"" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 40;
 found = ScmString "The letter 'A' can be entered as A"}
# test_string nt_sexpr "lambda" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 6; found = ScmSymbol "lambda"}
# test_string nt_sexpr "if" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 2; found = ScmSymbol "if"}
# test_string nt_sexpr "#\\space" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 7; found = ScmChar ' '}
# test_string nt_sexpr "#\\return" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 8; found = ScmChar '\r'}
# test_string nt_sexpr "#\\newline" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 9; found = ScmChar '\n'}
# test_string nt_sexpr "#\\tab" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 5; found = ScmChar '\t'}
# test_string nt_sexpr "#\\ponpon" 0;;
Exception: PC.X_no_match.
# test_string nt_sexpr "#\\gafrur" 0;;
Exception: PC.X_no_match.
# test_string nt_sexpr "#\\\\" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 3; found = ScmChar '\\'}
# test_string nt_sexpr "#\\\"" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 3; found = ScmChar '"'}
# test_string nt_sexpr "#\\x41" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 5; found = ScmChar 'A'}
# test_string nt_sexpr "#\\x20" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 5; found = ScmChar ' '}
# test_string nt_sexpr "#\\x61" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 5; found = ScmChar 'a'}
# test_string nt_sexpr "1234" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 4; found = ScmNumber (ScmRational (1234, 1))}
# test_string nt_sexpr "00001234" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 8; found = ScmNumber (ScmRational (1234, 1))}
# test_string nt_sexpr "00001234e0" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 10; found = ScmNumber (ScmReal 1234.)}
# test_string nt_sexpr "2/3" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 3; found = ScmNumber (ScmRational (2, 3))}
# test_string nt_sexpr "2/0" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 3; found = ScmSymbol "2/0"}
# test_string nt_sexpr "2/6" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 3; found = ScmNumber (ScmRational (1, 3))}
# test_string nt_sexpr "1.234" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 5; found = ScmNumber (ScmReal 1.234)}
# test_string nt_sexpr "1.234e1" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 7; found = ScmNumber (ScmReal 12.34)}
# test_string nt_sexpr "1.234e+1" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 8; found = ScmNumber (ScmReal 12.34)}
# test_string nt_sexpr "1.234*10^+1" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 11; found = ScmNumber (ScmReal 12.34)}
# test_string nt_sexpr "1.234*10^1" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 10; found = ScmNumber (ScmReal 12.34)}
# test_string nt_sexpr "1.234*10^-1" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 11;
 found = ScmNumber (ScmReal 0.12340000000000001)}
# test_string nt_sexpr ".1234e-10" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 9; found = ScmNumber (ScmReal 1.234e-11)}
# test_string nt_sexpr ".1234*10**-10" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 13; found = ScmNumber (ScmReal 1.234e-11)}
# test_string nt_sexpr ".1234*10^-10" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 12; found = ScmNumber (ScmReal 1.234e-11)}
# test_string nt_sexpr "-.1234*10^-10" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 13; found = ScmNumber (ScmReal (-1.234e-11))}
# test_string nt_sexpr "()" 0;;
- : sexpr PC.parsing_result = {index_from = 0; index_to = 2; found = ScmNil}
# test_string nt_sexpr "#()" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 3; found = ScmVector []}
# test_string nt_sexpr "(1 . 2)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 7;
 found =
  ScmPair (ScmNumber (ScmRational (1, 1)), ScmNumber (ScmRational (2, 1)))}
# test_string nt_sexpr "(1.2)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 5;
 found = ScmPair (ScmNumber (ScmReal 1.2), ScmNil)}
# test_string nt_sexpr "#(1.2)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 6; found = ScmVector [ScmNumber (ScmReal 1.2)]}
# test_string nt_sexpr "#(1 2)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 6;
 found =
  ScmVector [ScmNumber (ScmRational (1, 1)); ScmNumber (ScmRational (2, 1))]}
# test_string nt_sexpr "#(a b c)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 8;
 found = ScmVector [ScmSymbol "a"; ScmSymbol "b"; ScmSymbol "c"]}
# test_string nt_sexpr "(a b c)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 7;
 found =
  ScmPair
   (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmNil)))}
# test_string nt_sexpr "(a b . c)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 9;
 found = ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmSymbol "c"))}
# test_string nt_sexpr "((a . #t) (b . #f))" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 19;
 found =
  ScmPair
   (ScmPair (ScmSymbol "a", ScmBoolean true),
    ScmPair (ScmPair (ScmSymbol "b", ScmBoolean false), ScmNil))}
# test_string nt_sexpr "   #(   )   " 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 12; found = ScmVector []}
# test_string nt_sexpr "    (   )   " 0;;
- : sexpr PC.parsing_result = {index_from = 0; index_to = 12; found = ScmNil}
# test_string nt_sexpr "(define a 3)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 12;
 found =
  ScmPair
   (ScmSymbol "define",
    ScmPair (ScmSymbol "a", ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil)))}
# test_string nt_sexpr "\"~{(+ 2 3)}\"" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 12;
 found =
  ScmPair
   (ScmSymbol "format",
    ScmPair
     (ScmString "~a",
      ScmPair
       (ScmPair
         (ScmSymbol "+",
          ScmPair
           (ScmNumber (ScmRational (2, 1)),
            ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil))),
        ScmNil)))}
# test_string nt_sexpr "\"~{   (+ 2 3)    }\"" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 19;
 found =
  ScmPair
   (ScmSymbol "format",
    ScmPair
     (ScmString "~a",
      ScmPair
       (ScmPair
         (ScmSymbol "+",
          ScmPair
           (ScmNumber (ScmRational (2, 1)),
            ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil))),
        ScmNil)))}
# test_string nt_sexpr "`(,a ,@b)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 9;
 found =
  ScmPair
   (ScmSymbol "quasiquote",
    ScmPair
     (ScmPair
       (ScmPair (ScmSymbol "unquote", ScmPair (ScmSymbol "a", ScmNil)),
        ScmPair
         (ScmPair
           (ScmSymbol "unquote-splicing", ScmPair (ScmSymbol "b", ScmNil)),
          ScmNil)),
      ScmNil))}
# test_string nt_sexpr "'a" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 2;
 found = ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "a", ScmNil))}
# test_string nt_sexpr "''a" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 3;
 found =
  ScmPair
   (ScmSymbol "quote",
    ScmPair
     (ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "a", ScmNil)), ScmNil))}
# test_string nt_sexpr "'''a" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 4;
 found =
  ScmPair
   (ScmSymbol "quote",
    ScmPair
     (ScmPair
       (ScmSymbol "quote",
        ScmPair
         (ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "a", ScmNil)),
          ScmNil)),
      ScmNil))}
# test_string nt_sexpr "```a" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 4;
 found =
  ScmPair
   (ScmSymbol "quasiquote",
    ScmPair
     (ScmPair
       (ScmSymbol "quasiquote",
        ScmPair
         (ScmPair (ScmSymbol "quasiquote", ScmPair (ScmSymbol "a", ScmNil)),
          ScmNil)),
      ScmNil))}
# test_string nt_sexpr ",@a" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 3;
 found =
  ScmPair (ScmSymbol "unquote-splicing", ScmPair (ScmSymbol "a", ScmNil))}
# test_string nt_sexpr ",@,@,@a" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 7;
 found =
  ScmPair
   (ScmSymbol "unquote-splicing",
    ScmPair
     (ScmPair
       (ScmSymbol "unquote-splicing",
        ScmPair
         (ScmPair
           (ScmSymbol "unquote-splicing", ScmPair (ScmSymbol "a", ScmNil)),
          ScmNil)),
      ScmNil))}
# test_string nt_sexpr "((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x)))" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 48;
 found =
  ScmPair
   (ScmPair
     (ScmSymbol "lambda",
      ScmPair
       (ScmPair (ScmSymbol "x", ScmNil),
        ScmPair
         (ScmPair
           (ScmSymbol "quasiquote",
            ScmPair
             (ScmPair
               (ScmPair
                 (ScmSymbol "unquote", ScmPair (ScmSymbol "x", ScmNil)),
                ScmPair
                 (ScmPair
                   (ScmSymbol "quote",
                    ScmPair
                     (ScmPair
                       (ScmSymbol "unquote", ScmPair (ScmSymbol "x", ScmNil)),
                      ScmNil)),
                  ScmNil)),
              ScmNil)),
          ScmNil))),
    ScmPair
     (ScmPair
       (ScmSymbol "quote",
        ScmPair
         (ScmPair
           (ScmSymbol "lambda",
            ScmPair
             (ScmPair (ScmSymbol "x", ScmNil),
              ScmPair
               (ScmPair
                 (ScmSymbol "quasiquote",
                  ScmPair
                   (ScmPair
                     (ScmPair
                       (ScmSymbol "unquote", ScmPair (ScmSymbol "x", ScmNil)),
                      ScmPair
                       (ScmPair
                         (ScmSymbol "quote",
                          ScmPair
                           (ScmPair
                             (ScmSymbol "unquote",
                              ScmPair (ScmSymbol "x", ScmNil)),
                            ScmNil)),
                        ScmNil)),
                    ScmNil)),
                ScmNil))),
          ScmNil)),
      ScmNil))}
# test_string nt_sexpr "\"2 + 3 = ~{(+ 2 3)}\"" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 20;
 found =
  ScmPair
   (ScmSymbol "string-append",
    ScmPair
     (ScmString "2 + 3 = ",
      ScmPair
       (ScmPair
         (ScmSymbol "format",
          ScmPair
           (ScmString "~a",
            ScmPair
             (ScmPair
               (ScmSymbol "+",
                ScmPair
                 (ScmNumber (ScmRational (2, 1)),
                  ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil))),
              ScmNil))),
        ScmNil)))}
# test_string nt_sexpr "\"This is static: ABC and this is dynamic: ~{\"even though the string is static *in Scheme*, it is interpolated, so we consider it dynamic...\"}\"" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 142;
 found =
  ScmPair
   (ScmSymbol "string-append",
    ScmPair
     (ScmString "This is static: ABC and this is dynamic: ",
      ScmPair
       (ScmPair
         (ScmSymbol "format",
          ScmPair
           (ScmString "~a",
            ScmPair
             (ScmString
               "even though the string is static *in Scheme*, it is interpolated, so we consider it dynamic...",
              ScmNil))),
        ScmNil)))}
# test_string nt_sexpr "\"static ~{'dynamic} more static ~{'(more dynamic!)} \"" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 53;
 found =
  ScmPair
   (ScmSymbol "string-append",
    ScmPair
     (ScmString "static ",
      ScmPair
       (ScmPair
         (ScmSymbol "format",
          ScmPair
           (ScmString "~a",
            ScmPair
             (ScmPair
               (ScmSymbol "quote", ScmPair (ScmSymbol "dynamic", ScmNil)),
              ScmNil))),
        ScmPair
         (ScmString " more static ",
          ScmPair
           (ScmPair
             (ScmSymbol "format",
              ScmPair
               (ScmString "~a",
                ScmPair
                 (ScmPair
                   (ScmSymbol "quote",
                    ScmPair
                     (ScmPair
                       (ScmSymbol "more",
                        ScmPair (ScmSymbol "dynamic!", ScmNil)),
                      ScmNil)),
                  ScmNil))),
            ScmPair (ScmString " ", ScmNil))))))}
# test_string nt_sexpr "

;;; This is a line comment!
#;\"and this is an S-expression (string) that is removed via a sexpr-comment!\"

(a b c
 mary had a little lambda!
 #;#;#;#;\"I bet you didn't realize that sexpr-comments\" 
 \"may be\" \"nested!\"
 \"so all four strings shall be dumped and not appear in the list!\"
)

" 0;;
                        - : sexpr PC.parsing_result =
{index_from = 0; index_to = 290;
 found =
  ScmPair
   (ScmSymbol "a",
    ScmPair
     (ScmSymbol "b",
      ScmPair
       (ScmSymbol "c",
        ScmPair
         (ScmSymbol "mary",
          ScmPair
           (ScmSymbol "had",
            ScmPair
             (ScmSymbol "a",
              ScmPair
               (ScmSymbol "little", ScmPair (ScmSymbol "lambda!", ScmNil))))))))}
# test_string nt_sexpr "(you should know {that you can have paired/matching comments too, and that these are entered using braces!})" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 108;
 found =
  ScmPair
   (ScmSymbol "you",
    ScmPair (ScmSymbol "should", ScmPair (ScmSymbol "know", ScmNil)))}
# test_string nt_sexpr "({and {that {these too}}} may be nested!)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 41;
 found =
  ScmPair
   (ScmSymbol "may",
    ScmPair (ScmSymbol "be", ScmPair (ScmSymbol "nested!", ScmNil)))}
# test_string nt_sexpr "(a {#\\}} b c)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 13;
 found =
  ScmPair
   (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmNil)))}
# test_string nt_sexpr "(a {#\\{} b c)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 13;
 found =
  ScmPair
   (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmNil)))}
# test_string nt_sexpr "(a {\"}}}}{{{{\"} b c)" 0;;
- : sexpr PC.parsing_result =
{index_from = 0; index_to = 20;
 found =
  ScmPair
   (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmNil)))}

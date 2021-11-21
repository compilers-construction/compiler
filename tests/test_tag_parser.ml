#use "tag-parser.ml";;

type case = {name: string; input: sexpr; expected: expr};;

let cases = [
 {name = "Boolean"; input = ScmBoolean true;
  expected = ScmConst (ScmBoolean true)};
 {name = "Char"; input = ScmChar 'a'; expected = ScmConst (ScmChar 'a')};
 {name = "String"; input = ScmString "test";
  expected = ScmConst (ScmString "test")};
 {name = "Rational"; input = ScmNumber (ScmRational (1, 2));
  expected = ScmConst (ScmNumber (ScmRational (1, 2)))};
 {name = "Real"; input = ScmNumber (ScmReal 3.1415);
  expected = ScmConst (ScmNumber (ScmReal 3.1415))};
 {name = "Vector";
  input =
   ScmPair
    (ScmSymbol "quote",
     ScmPair
      (ScmPair
        (ScmSymbol "quote", ScmPair (ScmVector [ScmNil; ScmChar '\n'], ScmNil)),
       ScmNil));
  expected =
   ScmConst
    (ScmPair
      (ScmSymbol "quote", ScmPair (ScmVector [ScmNil; ScmChar '\n'], ScmNil)))};
 {name = "Symbol"; input = ScmSymbol "TestSym"; expected = ScmVar "TestSym"};
 {name = "If";
  input =
   ScmPair
    (ScmSymbol "if",
     ScmPair
      (ScmPair (ScmSymbol "a", ScmNil),
       ScmPair (ScmString "then", ScmPair (ScmString "else", ScmNil))));
  expected =
   ScmIf (ScmApplic (ScmVar "a", []), ScmConst (ScmString "then"),
    ScmConst (ScmString "else"))};
 {name = "Seq";
  input =
   ScmPair
    (ScmSymbol "begin",
     ScmPair
      (ScmSymbol "TestVar1",
       ScmPair
        (ScmPair
          (ScmSymbol "if",
           ScmPair (ScmBoolean true, ScmPair (ScmSymbol "TestVar2", ScmNil))),
         ScmNil)));
  expected =
   ScmSeq
    [ScmVar "TestVar1";
     ScmIf (ScmConst (ScmBoolean true), ScmVar "TestVar2", ScmConst ScmVoid)]};
 {name = "Set";
  input =
   ScmPair
    (ScmSymbol "set!",
     ScmPair
      (ScmSymbol "TestVar",
       ScmPair
        (ScmPair
          (ScmSymbol "begin",
           ScmPair
            (ScmPair
              (ScmSymbol "display", ScmPair (ScmSymbol "TestVar", ScmNil)),
             ScmPair
              (ScmPair
                (ScmChar '\n', ScmPair (ScmNumber (ScmReal 0.1), ScmNil)),
               ScmNil))),
         ScmNil)));
  expected =
   ScmSet (ScmVar "TestVar",
    ScmSeq
     [ScmApplic (ScmVar "display", [ScmVar "TestVar"]);
      ScmApplic (ScmConst (ScmChar '\n'), [ScmConst (ScmNumber (ScmReal 0.1))])])};
 {name = "Def";
  input =
   ScmPair
    (ScmSymbol "define",
     ScmPair
      (ScmSymbol "TestVar1",
       ScmPair
        (ScmPair
          (ScmSymbol "set!",
           ScmPair
            (ScmSymbol "TestVar2",
             ScmPair (ScmNumber (ScmRational (-1, 1)), ScmNil))),
         ScmNil)));
  expected =
   ScmDef (ScmVar "TestVar1",
    ScmSet (ScmVar "TestVar2", ScmConst (ScmNumber (ScmRational (-1, 1)))))};
 {name = "Or";
  input =
   ScmPair
    (ScmSymbol "or",
     ScmPair
      (ScmSymbol "a",
       ScmPair
        (ScmPair
          (ScmSymbol "if",
           ScmPair
            (ScmSymbol "a",
             ScmPair (ScmBoolean false, ScmPair (ScmSymbol "a", ScmNil)))),
         ScmNil)));
  expected =
   ScmOr
    [ScmVar "a"; ScmIf (ScmVar "a", ScmConst (ScmBoolean false), ScmVar "a")]};
  {name = "LambdaSimple";
  input =
   ScmPair
    (ScmSymbol "lambda",
     ScmPair
      (ScmPair
        (ScmSymbol "var1",
         ScmPair (ScmSymbol "var2", ScmPair (ScmSymbol "var3", ScmNil))),
       ScmPair
        (ScmPair
          (ScmSymbol "if",
           ScmPair
            (ScmPair
              (ScmSymbol "=",
               ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmNil))),
             ScmPair
              (ScmPair
                (ScmSymbol "cons",
                 ScmPair
                  (ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "a", ScmNil)),
                   ScmPair
                    (ScmPair
                      (ScmSymbol "quote", ScmPair (ScmSymbol "a", ScmNil)),
                     ScmNil))),
               ScmPair
                (ScmPair
                  (ScmSymbol "cons",
                   ScmPair
                    (ScmPair
                      (ScmSymbol "quote", ScmPair (ScmSymbol "a", ScmNil)),
                     ScmPair
                      (ScmPair
                        (ScmSymbol "quote", ScmPair (ScmSymbol "b", ScmNil)),
                       ScmNil))),
                 ScmNil)))),
         ScmNil)));
  expected =
   ScmLambdaSimple (["var1"; "var2"; "var3"],
    ScmIf (ScmApplic (ScmVar "=", [ScmVar "a"; ScmVar "b"]),
     ScmApplic (ScmVar "cons",
      [ScmConst (ScmSymbol "a"); ScmConst (ScmSymbol "a")]),
     ScmApplic (ScmVar "cons",
      [ScmConst (ScmSymbol "a"); ScmConst (ScmSymbol "b")])))};
 {name = "LambdaSimple-implicit-seq";
  input =
   ScmPair
    (ScmSymbol "lambda",
     ScmPair
      (ScmPair
        (ScmSymbol "var1",
         ScmPair (ScmSymbol "var2", ScmPair (ScmSymbol "var3", ScmNil))),
       ScmPair
        (ScmSymbol "var1",
         ScmPair (ScmSymbol "var2", ScmPair (ScmSymbol "var3", ScmNil)))));
  expected =
   ScmLambdaSimple (["var1"; "var2"; "var3"],
    ScmSeq [ScmVar "var1"; ScmVar "var2"; ScmVar "var3"])};
 {name = "LambdaOpt";
  input =
   ScmPair
    (ScmSymbol "lambda",
     ScmPair
      (ScmPair
        (ScmSymbol "var1", ScmPair (ScmSymbol "Var2", ScmSymbol "OptVar")),
       ScmPair
        (ScmPair
          (ScmSymbol "lambda",
           ScmPair
            (ScmPair (ScmSymbol "Var3", ScmSymbol "OptVar"),
             ScmPair (ScmSymbol "OptVar", ScmNil))),
         ScmNil)));
  expected =
   ScmLambdaOpt (["var1"; "Var2"], "OptVar",
    ScmLambdaOpt (["Var3"], "OptVar", ScmVar "OptVar"))};
 {name = "LambdaOpt-implicit-seq";
  input =
   ScmPair
    (ScmSymbol "lambda",
     ScmPair
      (ScmPair
        (ScmSymbol "var1", ScmPair (ScmSymbol "Var2", ScmSymbol "OptVar")),
       ScmPair
        (ScmPair
          (ScmSymbol "lambda",
           ScmPair
            (ScmPair (ScmSymbol "Var3", ScmSymbol "OptVar"),
             ScmPair (ScmSymbol "OptVar", ScmPair (ScmSymbol "Var3", ScmNil)))),
         ScmNil)));
  expected =
   ScmLambdaOpt (["var1"; "Var2"], "OptVar",
    ScmLambdaOpt (["Var3"], "OptVar", ScmSeq [ScmVar "OptVar"; ScmVar "Var3"]))};
  {name = "LambdaVar";
  input =
   ScmPair
    (ScmSymbol "lambda",
     ScmPair
      (ScmSymbol "var", ScmPair (ScmPair (ScmSymbol "var", ScmNil), ScmNil)));
  expected = ScmLambdaOpt ([], "var", ScmApplic (ScmVar "var", []))};
 {name = "LambdaVar-implicit-seq";
  input =
   ScmPair
    (ScmSymbol "lambda",
     ScmPair
      (ScmSymbol "var",
       ScmPair
        (ScmPair
          (ScmSymbol "set!",
           ScmPair (ScmSymbol "var", ScmPair (ScmSymbol "+", ScmNil))),
         ScmPair
          (ScmPair
            (ScmSymbol "or",
             ScmPair
              (ScmPair (ScmSymbol "var", ScmNil),
               ScmPair (ScmPair (ScmSymbol "var", ScmNil), ScmNil))),
           ScmNil))));
  expected =
   ScmLambdaOpt ([], "var",
    ScmSeq
     [ScmSet (ScmVar "var", ScmVar "+");
      ScmOr [ScmApplic (ScmVar "var", []); ScmApplic (ScmVar "var", [])]])};
 {name = "Or-nil"; input = ScmPair (ScmSymbol "or", ScmNil);
  expected = ScmConst (ScmBoolean false)};
 {name = "Or-single";
  input = ScmPair (ScmSymbol "or", ScmPair (ScmString "or", ScmNil));
  expected = ScmConst (ScmString "or")};
 {name = "And";
  input =
   ScmPair
    (ScmSymbol "and",
     ScmPair
      (ScmPair (ScmSymbol "or", ScmPair (ScmSymbol "var", ScmNil)),
       ScmPair (ScmBoolean true, ScmPair (ScmSymbol "var", ScmNil))));
  expected =
   ScmIf (ScmVar "var",
    ScmIf (ScmConst (ScmBoolean true), ScmVar "var",
     ScmConst (ScmBoolean false)),
    ScmConst (ScmBoolean false))};
 {name = "MIT-define-var";
  input =
   ScmPair
    (ScmSymbol "define",
     ScmPair
      (ScmPair (ScmSymbol "list", ScmSymbol "l"),
       ScmPair (ScmSymbol "l", ScmNil)));
  expected = ScmDef (ScmVar "list", ScmLambdaOpt ([], "l", ScmVar "l"))};
 {name = "MIT-define-opt";
  input =
   ScmPair
    (ScmSymbol "define",
     ScmPair
      (ScmPair (ScmSymbol "facts", ScmPair (ScmSymbol "n", ScmSymbol "ns")),
       ScmPair
        (ScmPair
          (ScmSymbol "if",
           ScmPair
            (ScmPair (ScmSymbol "zero?", ScmPair (ScmSymbol "n", ScmNil)),
             ScmPair
              (ScmPair
                (ScmSymbol "if",
                 ScmPair
                  (ScmPair
                    (ScmSymbol "null?", ScmPair (ScmSymbol "ns", ScmNil)),
                   ScmPair
                    (ScmNumber (ScmRational (1, 1)),
                     ScmPair
                      (ScmPair
                        (ScmSymbol "apply",
                         ScmPair
                          (ScmSymbol "facts",
                           ScmPair
                            (ScmPair
                              (ScmSymbol "car",
                               ScmPair (ScmSymbol "ns", ScmNil)),
                             ScmPair
                              (ScmPair
                                (ScmSymbol "cdr",
                                 ScmPair (ScmSymbol "ns", ScmNil)),
                               ScmNil)))),
                       ScmNil)))),
               ScmPair
                (ScmPair
                  (ScmSymbol "*",
                   ScmPair
                    (ScmSymbol "n",
                     ScmPair
                      (ScmPair
                        (ScmSymbol "apply",
                         ScmPair
                          (ScmSymbol "facts",
                           ScmPair
                            (ScmPair
                              (ScmSymbol "-",
                               ScmPair
                                (ScmSymbol "n",
                                 ScmPair
                                  (ScmNumber (ScmRational (1, 1)), ScmNil))),
                             ScmPair (ScmSymbol "ns", ScmNil)))),
                       ScmNil))),
                 ScmNil)))),
         ScmNil)));
  expected =
   ScmDef (ScmVar "facts",
    ScmLambdaOpt (["n"], "ns",
     ScmIf (ScmApplic (ScmVar "zero?", [ScmVar "n"]),
      ScmIf (ScmApplic (ScmVar "null?", [ScmVar "ns"]),
       ScmConst (ScmNumber (ScmRational (1, 1))),
       ScmApplic (ScmVar "apply",
        [ScmVar "facts"; ScmApplic (ScmVar "car", [ScmVar "ns"]);
         ScmApplic (ScmVar "cdr", [ScmVar "ns"])])),
      ScmApplic (ScmVar "*",
       [ScmVar "n";
        ScmApplic (ScmVar "apply",
         [ScmVar "facts";
          ScmApplic (ScmVar "-",
           [ScmVar "n"; ScmConst (ScmNumber (ScmRational (1, 1)))]);
          ScmVar "ns"])]))))};
 {name = "MIT-define";
  input =
   ScmPair
    (ScmSymbol "define",
     ScmPair
      (ScmPair (ScmSymbol "foo", ScmPair (ScmSymbol "sym", ScmNil)),
       ScmPair
        (ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "sym", ScmNil)),
         ScmNil)));
  expected =
   ScmDef (ScmVar "foo", ScmLambdaSimple (["sym"], ScmConst (ScmSymbol "sym")))};
 {name = "Let";
  input =
   ScmPair
    (ScmSymbol "let",
     ScmPair
      (ScmPair
        (ScmPair
          (ScmSymbol "var1",
           ScmPair
            (ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "value1", ScmNil)),
             ScmNil)),
         ScmPair
          (ScmPair
            (ScmSymbol "var2",
             ScmPair
              (ScmPair
                (ScmSymbol "and",
                 ScmPair
                  (ScmSymbol "value2", ScmPair (ScmSymbol "value3", ScmNil))),
               ScmNil)),
           ScmNil)),
       ScmPair (ScmSymbol "body1", ScmPair (ScmSymbol "body2", ScmNil))));
  expected =
   ScmApplic
    (ScmLambdaSimple (["var1"; "var2"],
      ScmSeq [ScmVar "body1"; ScmVar "body2"]),
    [ScmConst (ScmSymbol "value1");
     ScmIf (ScmVar "value2", ScmVar "value3", ScmConst (ScmBoolean false))])};
 {name = "Let*-nil";
  input =
   ScmPair
    (ScmSymbol "let*",
     ScmPair
      (ScmNil,
       ScmPair
        (ScmPair
          (ScmSymbol "and",
           ScmPair
            (ScmSymbol "TestVar1", ScmPair (ScmSymbol "TestVar2", ScmNil))),
         ScmNil)));
  expected =
   ScmApplic
    (ScmLambdaSimple ([],
      ScmIf (ScmVar "TestVar1", ScmVar "TestVar2", ScmConst (ScmBoolean false))),
    [])};
 {name = "Let*";
  input =
   ScmPair
    (ScmSymbol "let*",
     ScmPair
      (ScmPair
        (ScmPair
          (ScmSymbol "a", ScmPair (ScmNumber (ScmRational (1, 1)), ScmNil)),
         ScmPair
          (ScmPair
            (ScmSymbol "b", ScmPair (ScmNumber (ScmRational (2, 1)), ScmNil)),
           ScmPair
            (ScmPair
              (ScmSymbol "c", ScmPair (ScmNumber (ScmRational (3, 2)), ScmNil)),
             ScmNil))),
       ScmPair
        (ScmPair
          (ScmSymbol "+",
           ScmPair
            (ScmSymbol "a",
             ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmNil)))),
         ScmNil)));
  expected =
   ScmApplic
    (ScmLambdaSimple (["a"],
      ScmApplic
       (ScmLambdaSimple (["b"],
         ScmApplic
          (ScmLambdaSimple (["c"],
            ScmApplic (ScmVar "+", [ScmVar "a"; ScmVar "b"; ScmVar "c"])),
          [ScmConst (ScmNumber (ScmRational (3, 2)))])),
       [ScmConst (ScmNumber (ScmRational (2, 1)))])),
    [ScmConst (ScmNumber (ScmRational (1, 1)))])};
 {name = "Letrec-nil";
  input =
   ScmPair
    (ScmSymbol "letrec",
     ScmPair
      (ScmNil,
       ScmPair
        (ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "a", ScmNil)), ScmNil)));
  expected =
   ScmApplic (ScmLambdaSimple ([], ScmApplic (ScmVar "a", [ScmVar "a"])), [])};
 {name = "Letrec";
  input =
   ScmPair
    (ScmSymbol "letrec",
     ScmPair
      (ScmPair
        (ScmPair
          (ScmSymbol "fact",
           ScmPair
            (ScmPair
              (ScmSymbol "lambda",
               ScmPair
                (ScmSymbol "n",
                 ScmPair
                  (ScmPair
                    (ScmSymbol "let",
                     ScmPair
                      (ScmPair
                        (ScmPair
                          (ScmSymbol "n",
                           ScmPair
                            (ScmPair
                              (ScmSymbol "car",
                               ScmPair (ScmSymbol "n", ScmNil)),
                             ScmNil)),
                         ScmNil),
                       ScmPair
                        (ScmPair
                          (ScmSymbol "if",
                           ScmPair
                            (ScmPair
                              (ScmSymbol "zero?",
                               ScmPair (ScmSymbol "n", ScmNil)),
                             ScmPair
                              (ScmNumber (ScmRational (1, 1)),
                               ScmPair
                                (ScmPair
                                  (ScmSymbol "*",
                                   ScmPair
                                    (ScmSymbol "n",
                                     ScmPair
                                      (ScmPair
                                        (ScmSymbol "fact",
                                         ScmPair
                                          (ScmPair
                                            (ScmSymbol "-",
                                             ScmPair
                                              (ScmSymbol "n",
                                               ScmPair
                                                (ScmNumber (ScmRational (1, 1)),
                                                 ScmNil))),
                                           ScmNil)),
                                       ScmNil))),
                                 ScmNil)))),
                         ScmNil))),
                   ScmNil))),
             ScmNil)),
         ScmNil),
       ScmPair
        (ScmPair
          (ScmSymbol "fact", ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil)),
         ScmNil)));
  expected =
   ScmApplic
    (ScmLambdaSimple (["fact"],
      ScmSeq
       [ScmSet (ScmVar "fact",
         ScmLambdaOpt ([], "n",
          ScmApplic
           (ScmLambdaSimple (["n"],
             ScmIf (ScmApplic (ScmVar "zero?", [ScmVar "n"]),
              ScmConst (ScmNumber (ScmRational (1, 1))),
              ScmApplic (ScmVar "*",
               [ScmVar "n";
                ScmApplic (ScmVar "fact",
                 [ScmApplic (ScmVar "-",
                   [ScmVar "n"; ScmConst (ScmNumber (ScmRational (1, 1)))])])]))),
           [ScmApplic (ScmVar "car", [ScmVar "n"])])));
        ScmApplic (ScmVar "fact", [ScmConst (ScmNumber (ScmRational (3, 1)))])]),
    [ScmConst (ScmSymbol "whatever")])};
 {name = "Cond";
  input =
   ScmPair
    (ScmSymbol "cond",
     ScmPair
      (ScmPair
        (ScmSymbol "var-a",
         ScmPair (ScmSymbol "var-b", ScmPair (ScmSymbol "var-c", ScmNil))),
       ScmPair
        (ScmPair (ScmSymbol "var-d", ScmPair (ScmSymbol "var-e", ScmNil)),
         ScmPair
          (ScmPair (ScmBoolean false, ScmPair (ScmBoolean true, ScmNil)),
           ScmNil))));
  expected =
   ScmIf (ScmVar "var-a", ScmSeq [ScmVar "var-b"; ScmVar "var-c"],
    ScmIf (ScmVar "var-d", ScmVar "var-e",
     ScmIf (ScmConst (ScmBoolean false), ScmConst (ScmBoolean true),
      ScmConst ScmVoid)))};
 {name = "Cond-else";
  input =
   ScmPair
    (ScmSymbol "cond",
     ScmPair
      (ScmPair
        (ScmSymbol "var-a",
         ScmPair (ScmSymbol "var-b", ScmPair (ScmSymbol "var-c", ScmNil))),
       ScmPair
        (ScmPair (ScmSymbol "var-d", ScmPair (ScmSymbol "var-e", ScmNil)),
         ScmPair
          (ScmPair
            (ScmSymbol "else",
             ScmPair
              (ScmPair
                (ScmSymbol "and",
                 ScmPair (ScmNumber (ScmRational (1, 1)), ScmNil)),
               ScmNil)),
           ScmPair
            (ScmPair (ScmBoolean false, ScmPair (ScmBoolean true, ScmNil)),
             ScmNil)))));
  expected =
   ScmIf (ScmVar "var-a", ScmSeq [ScmVar "var-b"; ScmVar "var-c"],
    ScmIf (ScmVar "var-d", ScmVar "var-e",
     ScmConst (ScmNumber (ScmRational (1, 1)))))};
 {name = "Cond-arrow";
  input =
   ScmPair
    (ScmSymbol "define",
     ScmPair
      (ScmPair (ScmSymbol "foo", ScmPair (ScmSymbol "b", ScmNil)),
       ScmPair
        (ScmPair
          (ScmSymbol "let",
           ScmPair
            (ScmPair
              (ScmPair
                (ScmSymbol "b",
                 ScmPair
                  (ScmPair (ScmSymbol "not", ScmPair (ScmSymbol "b", ScmNil)),
                   ScmNil)),
               ScmNil),
             ScmPair
              (ScmPair
                (ScmSymbol "cond",
                 ScmPair
                  (ScmPair
                    (ScmSymbol "b",
                     ScmPair
                      (ScmSymbol "=>", ScmPair (ScmSymbol "foo", ScmNil))),
                   ScmPair
                    (ScmPair
                      (ScmSymbol "else",
                       ScmPair
                        (ScmPair
                          (ScmSymbol "display",
                           ScmPair (ScmSymbol "b", ScmNil)),
                         ScmNil)),
                     ScmNil))),
               ScmNil))),
         ScmNil)));
  expected =
   ScmDef (ScmVar "foo",
    ScmLambdaSimple (["b"],
     ScmApplic
      (ScmLambdaSimple (["b"],
        ScmApplic
         (ScmLambdaSimple (["value"; "f"; "rest"],
           ScmIf (ScmVar "value",
            ScmApplic (ScmApplic (ScmVar "f", []), [ScmVar "value"]),
            ScmApplic (ScmVar "reset", []))),
         [ScmVar "b"; ScmLambdaSimple ([], ScmVar "foo");
          ScmLambdaSimple ([], ScmApplic (ScmVar "display", [ScmVar "b"]))])),
      [ScmApplic (ScmVar "not", [ScmVar "b"])])))};
 {name = "QQ";
  input = ScmPair (ScmSymbol "quasiquote", ScmPair (ScmSymbol "a", ScmNil));
  expected = ScmConst (ScmSymbol "a")};
 {name = "QQ-nil";
  input = ScmPair (ScmSymbol "quasiquote", ScmPair (ScmNil, ScmNil));
  expected = ScmConst ScmNil};
 {name = "QQ-unquote";
  input =
   ScmPair
    (ScmSymbol "quasiquote",
     ScmPair
      (ScmPair (ScmSymbol "unquote", ScmPair (ScmSymbol "a", ScmNil)), ScmNil));
  expected = ScmVar "a"};
 {name = "QQ-unquote-splicing";
  input =
   ScmPair
    (ScmSymbol "quasiquote",
     ScmPair
      (ScmPair
        (ScmSymbol "unquote-splicing",
         ScmPair
          (ScmPair
            (ScmSymbol "+",
             ScmPair
              (ScmNumber (ScmRational (1, 1)),
               ScmPair (ScmNumber (ScmRational (2, 1)), ScmNil))),
           ScmNil)),
       ScmNil));
  expected =
   ScmConst
    (ScmPair
      (ScmSymbol "unquote-splicing",
       ScmPair
        (ScmPair
          (ScmSymbol "+",
           ScmPair
            (ScmNumber (ScmRational (1, 1)),
             ScmPair (ScmNumber (ScmRational (2, 1)), ScmNil))),
         ScmNil)))};
 {name = "QQ-list";
  input =
   ScmPair
    (ScmSymbol "quasiquote",
     ScmPair
      (ScmPair
        (ScmChar 'a',
         ScmPair
          (ScmSymbol "b",
           ScmPair
            (ScmPair (ScmSymbol "unquote", ScmPair (ScmSymbol "c", ScmNil)),
             ScmPair
              (ScmPair
                (ScmSymbol "unquote-splicing", ScmPair (ScmSymbol "d", ScmNil)),
               ScmPair
                (ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "e", ScmNil)),
                 ScmPair (ScmPair (ScmSymbol "f", ScmNil), ScmNil)))))),
       ScmNil));
  expected =
   ScmApplic (ScmVar "cons",
    [ScmConst (ScmChar 'a');
     ScmApplic (ScmVar "cons",
      [ScmConst (ScmSymbol "b");
       ScmApplic (ScmVar "cons",
        [ScmVar "c";
         ScmApplic (ScmVar "append",
          [ScmVar "d";
           ScmApplic (ScmVar "cons",
            [ScmApplic (ScmVar "cons",
              [ScmConst (ScmSymbol "quote");
               ScmApplic (ScmVar "cons",
                [ScmConst (ScmSymbol "e"); ScmConst ScmNil])]);
             ScmApplic (ScmVar "cons",
              [ScmApplic (ScmVar "cons",
                [ScmConst (ScmSymbol "f"); ScmConst ScmNil]);
               ScmConst ScmNil])])])])])])};
 {name = "QQ-vector";
  input =
   ScmPair
    (ScmSymbol "quasiquote",
     ScmPair
      (ScmVector
        [ScmSymbol "a";
         ScmPair (ScmSymbol "unquote", ScmPair (ScmSymbol "b", ScmNil));
         ScmPair
          (ScmSymbol "unquote-splicing", ScmPair (ScmSymbol "c", ScmNil));
         ScmString "d"],
       ScmNil));
  expected =
   ScmApplic (ScmVar "list->vector",
    [ScmApplic (ScmVar "cons",
      [ScmConst (ScmSymbol "a");
       ScmApplic (ScmVar "cons",
        [ScmVar "b";
         ScmApplic (ScmVar "append",
          [ScmVar "c";
           ScmApplic (ScmVar "cons",
            [ScmConst (ScmString "d"); ScmConst ScmNil])])])])])}
];;



let test_case case =
try
let actual = Tag_Parser.tag_parse_expression case.input in
if (expr_eq actual case.expected) then "PASS" else "FAILURE"
with
| X_syntax_error(s, msg) -> Printf.sprintf "Exception: Syntax Error message: %s for sexpr: %s" msg (string_of_sexpr s)
| X_reserved_word(s) -> Printf.sprintf "Exception: Reserved Word: %s" s
| X_not_implemented -> Printf.sprintf "Exception: Syntax not yet implemented"
| _ -> "Unknown Failure"

let test_cases cases =
let names, results =  (List.map (fun case -> case.name) cases),(List.map test_case cases) in
List.map2 (fun name result -> Printf.sprintf "%s: %s" result name) names results;;

List.map (Printf.printf "%s\n") (test_cases cases);;

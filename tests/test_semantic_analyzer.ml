
#use "semantic-analyser.ml";;

type 'a test_case = {name: string; test: 'a -> expr' ; input: 'a; expected: expr'}

type case =
| ExprCase of expr test_case
| Expr'Case of expr' test_case

let cases = [
ExprCase {name = "lexical annotation"; test = Semantic_Analysis.annotate_lexical_addresses;
input =
  ScmLambdaSimple (["x"],
   ScmApplic (ScmVar "list",
    [ScmApplic (ScmVar "+",
      [ScmVar "x"; ScmConst (ScmNumber (ScmRational (1, 1)))]);
     ScmLambdaSimple (["y"], ScmSet (ScmVar "x", ScmVar "y"))]));
expected =
   ScmLambdaSimple' (["x"],
   ScmApplic' (ScmVar' (VarFree "list"),
    [ScmApplic' (ScmVar' (VarFree "+"),
      [ScmVar' (VarParam ("x", 0)); ScmConst' (ScmNumber (ScmRational (1, 1)))]);
     ScmLambdaSimple' (["y"],
      ScmSet' (VarBound ("x", 0, 0), ScmVar' (VarParam ("y", 0))))]))};

Expr'Case {name = "TP annotation"; test = Semantic_Analysis.annotate_tail_calls;
input =
   ScmLambdaSimple' (["x"],
   ScmApplic' (ScmVar' (VarFree "list"),
    [ScmApplic' (ScmVar' (VarFree "+"),
      [ScmVar' (VarParam ("x", 0)); ScmConst' (ScmNumber (ScmRational (1, 1)))]);
     ScmLambdaSimple' (["y"],
      ScmSet' (VarBound ("x", 0, 0), ScmVar' (VarParam ("y", 0))))]));
expected =
   ScmLambdaSimple' (["x"],
    ScmApplicTP' (ScmVar' (VarFree "list"),
     [ScmApplic' (ScmVar' (VarFree "+"),
       [ScmVar' (VarParam ("x", 0)); ScmConst' (ScmNumber (ScmRational (1, 1)))]);
      ScmLambdaSimple' (["y"],
       ScmSet' (VarBound ("x", 0, 0), ScmVar' (VarParam ("y", 0))))]))};

Expr'Case {name = "box"; test = Semantic_Analysis.box_set;
input =
   ScmLambdaSimple' (["x"],
       ScmApplicTP' (ScmVar' (VarFree "list"),
        [ScmApplic' (ScmVar' (VarFree "+"),
          [ScmVar' (VarParam ("x", 0)); ScmConst' (ScmNumber (ScmRational (1, 1)))]);
         ScmLambdaSimple' (["y"],
          ScmSet' (VarBound ("x", 0, 0), ScmVar' (VarParam ("y", 0))))]));
expected =
  ScmLambdaSimple' (["x"],
   ScmSeq'
    [ScmSet' (VarParam ("x", 0), ScmBox' (VarParam ("x", 0)));
     ScmApplicTP' (ScmVar' (VarFree "list"),
      [ScmApplic' (ScmVar' (VarFree "+"),
        [ScmBoxGet' (VarParam ("x", 0));
         ScmConst' (ScmNumber (ScmRational (1, 1)))]);
       ScmLambdaSimple' (["y"],
        ScmBoxSet' (VarBound ("x", 0, 0), ScmVar' (VarParam ("y", 0))))])])};
];;


let case_name case =
match case with
| ExprCase c -> Printf.sprintf "Expr-%s" c.name
| Expr'Case c -> Printf.sprintf "Expr'-%s" c.name

let test_case case =

try
let actual, expected = match case with
| ExprCase c -> (c.test c.input), c.expected
| Expr'Case c -> (c.test c.input), c.expected in
if (expr'_eq actual expected) then "PASS" else "FAILURE"
with
| X_not_yet_implemented -> Printf.sprintf "Exception: Syntax not yet implemented"
| _ -> "Unknown Failure"

let test_cases cases =
let names, results =  (List.map case_name cases),(List.map test_case cases) in
List.map2 (fun name result -> Printf.sprintf "%s: %s" result name) names results;;

List.map (Printf.printf "%s\n") (test_cases cases);;

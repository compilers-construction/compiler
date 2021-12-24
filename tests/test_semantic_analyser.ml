
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


let var_eq v1 v2 =
match v1, v2 with
  | VarFree (name1), VarFree (name2) -> String.equal name1 name2
  | VarBound (name1, major1, minor1), VarBound (name2, major2, minor2) ->
    major1 = major2 && minor1 = minor2 && (String.equal name1 name2)
  | VarParam (name1, index1), VarParam (name2, index2) ->
       index1 = index2 && (String.equal name1 name2)
  | _ -> false

let list_eq eq l1 l2 = (List.length l1) = (List.length l2) && List.for_all2 eq l1 l2;;

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | ScmConst' (sexpr1), ScmConst' (sexpr2) -> sexpr_eq sexpr1 sexpr2
  | ScmVar' (var1), ScmVar' (var2) -> var_eq var1 var2
  | ScmIf' (test1, dit1, dif1), ScmIf' (test2, dit2, dif2) -> (expr'_eq test1 test2) &&
                                            (expr'_eq dit1 dit2) &&
                                              (expr'_eq dif1 dif2)
  | (ScmSeq' (exprs1), ScmSeq' (exprs2) | ScmOr' (exprs1), ScmOr' (exprs2)) ->
        list_eq expr'_eq exprs1 exprs2
  | (ScmSet' (var1, val1), ScmSet' (var2, val2) | ScmDef' (var1, val1), ScmDef' (var2, val2)) ->
        (var_eq var1 var2) && (expr'_eq val1 val2)
  | ScmLambdaSimple' (vars1, body1), ScmLambdaSimple' (vars2, body2) ->
     (list_eq String.equal vars1 vars2) && (expr'_eq body1 body2)
  | ScmLambdaOpt' (vars1, var1, body1), ScmLambdaOpt' (vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (list_eq String.equal vars1 vars2) && (expr'_eq body1 body2)
  | ScmApplic' (e1, args1), ScmApplic' (e2, args2) ->
     (expr'_eq e1 e2) && (list_eq expr'_eq args1 args2)
  | ScmApplicTP' (e1, args1), ScmApplicTP' (e2, args2) ->
      (expr'_eq e1 e2) && (list_eq expr'_eq args1 args2)
  | ScmBox' (v1), ScmBox' (v2) -> var_eq v1 v2
  | ScmBoxGet' (v1), ScmBoxGet' (v2) -> var_eq v1 v2
  | ScmBoxSet' (v1, e1), ScmBoxSet' (v2, e2) -> (var_eq v1 v2) && (expr'_eq e1 e2)
  | _ -> false;;

let unannotate_lexical_address = function
| (VarFree name | VarParam (name, _) | VarBound (name, _, _)) -> ScmVar name

let rec unanalyze expr' =
match expr' with
  | ScmConst' s -> ScmConst(s)
  | ScmVar' var -> unannotate_lexical_address var
  | ScmBox' var -> ScmApplic(ScmVar "box", [unannotate_lexical_address var])
  | ScmBoxGet' var -> unannotate_lexical_address var
  | ScmBoxSet' (var, expr') -> ScmSet (unannotate_lexical_address var, unanalyze expr')
  | ScmIf' (test, dit, dif) -> ScmIf (unanalyze test, unanalyze dit, unanalyze dif)
  | ScmSeq' expr's -> ScmSeq (List.map unanalyze expr's)
  | ScmSet' (var, expr') -> ScmSet (unannotate_lexical_address var, unanalyze expr')
  | ScmDef' (var, expr') -> ScmDef (unannotate_lexical_address var, unanalyze expr')
  | ScmOr' expr's -> ScmOr (List.map unanalyze expr's)
  | ScmLambdaSimple' (params, expr') ->
        ScmLambdaSimple (params, unanalyze expr')
  | ScmLambdaOpt'(params, param, expr') ->
        ScmLambdaOpt (params, param, unanalyze expr')
  | (ScmApplic' (expr', expr's) | ScmApplicTP' (expr', expr's)) ->
        ScmApplic (unanalyze expr', List.map unanalyze expr's);;

let string_of_expr' expr' =
    string_of_expr (unanalyze expr');;

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

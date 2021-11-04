#use "pc.ml";;
#use "reader.ml";;


let input_file = Sys.argv.(1);;

let read_test_file file = 
let channel = open_in file in
really_input_string channel (in_channel_length channel);;

let input = 
String.trim (read_test_file input_file);;

let run_reader string =
let nt_sexprs = PC.star Reader.nt_sexpr in
let nt_sexprs = PC.caten nt_sexprs PC.nt_end_of_input in
let nt_sexprs = PC.pack nt_sexprs (fun (s, _) -> s) in
let result = nt_sexprs string 0 in
result.found

let output =
let output_sexprs = run_reader input in
let output_sexprs_str = List.map string_of_sexpr output_sexprs in
String.concat " " output_sexprs_str;;

let scheme_eq_expr = Printf.sprintf "(equal? '(%s) '(%s))" input output;;
Printf.printf "`(%s: ,%s)" input_file scheme_eq_expr;;

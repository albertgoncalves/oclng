type stmt =
  | StmtDrop of expr_pos
  | StmtHold of expr_pos
  | StmtReturn of expr_pos
  | StmtLet of (string * expr_pos)

and expr =
  | ExprInt of int
  | ExprStr of string
  | ExprVar of string
  | ExprFn of func
  | ExprCall of (expr_pos * expr_pos list)
  | ExprSwitch of (expr_pos * stmt_pos list list)

and func =
  {
    label : string_pos;
    args : string_pos list;
    body : stmt_pos list;
  }

and stmt_pos = (stmt * int)

and expr_pos = (expr * int)

and string_pos = (string * int)

let encode (chars : char list) : string =
  let rec f (prev : char list) : char list -> char list =
    function
    | [] -> List.rev prev
    | '"' :: ',' :: '"' :: xs -> f prev xs
    | x :: xs -> f (x :: prev) xs in
  chars
  |> List.map
    (function
      | '\n' -> "10"
      | c -> Printf.sprintf "\"%c\"" c)
  |> String.concat ","
  |> String.to_seq
  |> List.of_seq
  |> f []
  |> List.to_seq
  |> String.of_seq

let show_string (str : string) : string =
  encode (List.of_seq (String.to_seq str))

let rec show_expr : expr_pos -> string =
  function
  | (ExprInt x, _) -> string_of_int x
  | (ExprStr "", _) -> "\"\""
  | (ExprStr x, _) -> show_string x
  | (ExprVar x, _) -> x
  | (ExprFn func, _) -> fst func.label
  | (ExprCall (expr, []), _) -> Printf.sprintf "(%s)" (show_expr expr)
  | (ExprCall (expr, args), _) ->
    Printf.sprintf "(%s %s)" (show_expr expr) (show_exprs args)
  | (ExprSwitch (expr, branches), _) ->
    Printf.sprintf
      "branch %s { %s }"
      (show_expr expr)
      (
        String.concat
          " } { "
          (
            List.map
              (fun s -> String.concat " " (List.map show_stmt s))
              branches
          )
      )

and show_exprs (exprs : expr_pos list) : string =
  String.concat " " (List.map show_expr exprs)

and show_stmt : stmt_pos -> string =
  function
  | (StmtDrop expr, _) -> Printf.sprintf "drop %s" (show_expr expr)
  | (StmtHold expr, _) -> Printf.sprintf "hold %s" (show_expr expr)
  | (StmtReturn expr, _) -> Printf.sprintf "return %s" (show_expr expr)
  | (StmtLet (label, expr), _) ->
    Printf.sprintf "let %s %s" label (show_expr expr)

let show_func (func : func) : string =
  Printf.sprintf
    "%s %s {\n    \
     %s\n\
     }\n"
    (fst func.label)
    (String.concat " " (List.map fst func.args))
    (String.concat "\n    " (List.map show_stmt func.body))

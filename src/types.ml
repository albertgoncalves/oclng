type stmt =
  | StmtDrop of expr
  | StmtHold of expr
  | StmtReturn of expr
  | StmtLet of (string * expr)

and expr =
  | ExprInt of int
  | ExprStr of string
  | ExprVar of string
  | ExprFn of func
  | ExprCall of (expr * expr list)
  | ExprSwitch of (expr * stmt list list)

and func =
  {
    label : string;
    args : string list;
    body : stmt list;
  }

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

let rec show_expr : expr -> string =
  function
  | ExprInt x -> string_of_int x
  | ExprStr x -> show_string x
  | ExprVar x -> x
  | ExprFn func -> func.label
  | ExprCall (expr, []) -> Printf.sprintf "(%s)" (show_expr expr)
  | ExprCall (expr, args) ->
    Printf.sprintf "(%s %s)" (show_expr expr) (show_exprs args)
  | ExprSwitch (expr, branches) ->
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

and show_exprs (exprs : expr list) : string =
  String.concat " " (List.map show_expr exprs)

and show_stmt : stmt -> string =
  function
  | StmtDrop expr -> Printf.sprintf "drop %s" (show_expr expr)
  | StmtHold expr -> Printf.sprintf "hold %s" (show_expr expr)
  | StmtReturn expr -> Printf.sprintf "return %s" (show_expr expr)
  | StmtLet (label, expr) -> Printf.sprintf "let %s %s" label (show_expr expr)

let show_func (func : func) : string =
  Printf.sprintf
    "%s %s {\n    \
     %s\n\
     }\n"
    func.label
    (String.concat " " func.args)
    (String.concat "\n    " (List.map show_stmt func.body))

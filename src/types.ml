type bin_op =
  | BinOpEq
  | BinOpAdd
  | BinOpSub

type intrin =
  | IntrinPrintf
  | IntrinPack

type call =
  | CallIntrin of intrin
  | CallLabel of string

type branch = (string list) * (expr list)

and expr =
  | ExprDrop of expr
  | ExprRet of expr
  | ExprInt of int
  | ExprStr of string
  | ExprVar of string
  | ExprAssign of (string * expr)
  | ExprInject of (expr * int * expr)
  | ExprIf of (expr * expr list)
  | ExprIfThen of (expr * expr list * expr list)
  | ExprBinOp of (bin_op * expr * expr)
  | ExprCall of (bool * call * expr list)
  | ExprUnpack of (expr * branch list)

type func =
  {
    label : string;
    args : string list;
    body : expr list;
  }

let encode (chars : char list) : string =
  let rec f : char list -> char list =
    function
    | [] -> []
    | '"' :: ',' :: '"' :: xs -> f xs
    | x :: xs -> x :: f xs in
  chars
  |> List.map
    (function
      | '\n' -> "10"
      | c -> Printf.sprintf "\"%c\"" c)
  |> String.concat ","
  |> String.to_seq
  |> List.of_seq
  |> f
  |> List.to_seq
  |> String.of_seq

let show_string (str : string) : string =
  encode (List.of_seq (String.to_seq str))

let show_bin_op : bin_op -> string =
  function
  | BinOpEq -> "="
  | BinOpAdd -> "+"
  | BinOpSub -> "-"

let show_call : call -> string =
  function
  | CallIntrin IntrinPrintf -> "printf"
  | CallIntrin IntrinPack -> "pack"
  | CallLabel label -> label

let rec show_expr : expr -> string =
  function
  | ExprDrop expr -> Printf.sprintf "drop %s" (show_expr expr)
  | ExprRet expr -> Printf.sprintf "return %s" (show_expr expr)
  | ExprInt x -> string_of_int x
  | ExprStr x -> show_string x
  | ExprVar x -> x
  | ExprAssign (label, expr) ->
    Printf.sprintf "let %s %s" label (show_expr expr)
  | ExprInject (pointer, n, replacement) ->
    Printf.sprintf
      "inject %s %d %s"
      (show_expr pointer)
      n
      (show_expr replacement)
  | ExprIf (condition, exprs) ->
    Printf.sprintf "if %s { %s }" (show_expr condition) (show_exprs exprs)
  | ExprIfThen (condition, exprs_then, exprs_else) ->
    Printf.sprintf
      "if %s { %s } else { %s }"
      (show_expr condition)
      (show_exprs exprs_then)
      (show_exprs exprs_else)
  | ExprBinOp (op, l, r) ->
    Printf.sprintf "(%s %s %s)" (show_bin_op op) (show_expr l) (show_expr r)
  | ExprCall (tail, call, []) ->
    Printf.sprintf
      (
        if tail then
          "tail (%s)"
        else
          "(%s)"
      )
      (show_call call)
  | ExprCall (tail, call, args) ->
    Printf.sprintf
      (
        if tail then
          "tail (%s %s)"
        else
          "(%s %s)"
      )
      (show_call call)
      (show_exprs args)
  | ExprUnpack (packed, branches) ->
    Printf.sprintf
      "unpack %s { %s }"
      (show_expr packed)
      (show_branches branches)

and show_exprs (exprs : expr list) : string =
  String.concat " " (List.map show_expr exprs)

and show_branch ((args, exprs) : branch) : string =
  Printf.sprintf "%s { %s }" (String.concat " " args) (show_exprs exprs)

and show_branches (branches : branch list) : string =
  String.concat "; " (List.map show_branch branches)

let show_func (func : func) : string =
  Printf.sprintf
    "%s %s {\n    \
     %s\n\
     }\n"
    func.label
    (String.concat " " func.args)
    (String.concat "\n    " (List.map show_expr func.body))

open Types

type token =
  | TokenLParen
  | TokenRParen
  | TokenLBrace
  | TokenRBrace
  | TokenEq
  | TokenAdd
  | TokenSub
  | TokenLet
  | TokenIf
  | TokenElse
  | TokenUnpack
  | TokenInt of int
  | TokenIdent of string
  | TokenStr of string

let show_token : token -> string =
  function
  | TokenLParen -> "("
  | TokenRParen -> ")"
  | TokenLBrace -> "{"
  | TokenRBrace -> "}"
  | TokenEq -> "="
  | TokenAdd -> "+"
  | TokenSub -> "-"
  | TokenLet -> "let"
  | TokenIf -> "if"
  | TokenElse -> "else"
  | TokenUnpack -> "unpack"
  | TokenInt x -> string_of_int x
  | TokenIdent x -> x
  | TokenStr x -> Printf.sprintf "\"%s\"" x

let is_digit : char -> bool =
  function
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
  | _ -> false

let is_space : char -> bool =
  function
  | '\n' | '\t' | ' ' -> true
  | _ -> false

let into_token : string -> token =
  function
  | "(" -> TokenLParen
  | ")" -> TokenRParen
  | "{" -> TokenLBrace
  | "}" -> TokenRBrace
  | "=" -> TokenEq
  | "+" -> TokenAdd
  | "-" -> TokenSub
  | "let" -> TokenLet
  | "if" -> TokenIf
  | "else" -> TokenElse
  | "unpack" -> TokenUnpack
  | cs ->
    (
      assert ((String.length cs) <> 0);
      if String.for_all is_digit cs then
        TokenInt (int_of_string cs)
      else
        let n : int = String.length cs in
        if (cs.[0] = '"') && (cs.[n - 1] = '"') then (
          TokenStr (String.sub cs 1 (n - 2))
        ) else (
          assert (String.for_all (fun x -> not (is_space x)) cs);
          TokenIdent cs
        )
    )

let tokenize (source : bytes) : token Queue.t =
  let n : int = Bytes.length source in
  let tokens : string Queue.t = Queue.create () in
  let rec loop_string (buffer : Buffer.t) (i : int) : int =
    match Bytes.get source i with
    | '\\' -> loop_string_escaped buffer (i + 1)
    | '"' ->
      (
        Buffer.add_char buffer '"';
        i + 1
      )
    | c ->
      (
        Buffer.add_char buffer c;
        loop_string buffer (i + 1)
      )
  and loop_string_escaped (buffer : Buffer.t) (i : int) : int =
    match Bytes.get source i with
    | 'n' ->
      (
        Buffer.add_char buffer '\n';
        loop_string buffer (i + 1)
      )
    | '"' ->
      (
        Buffer.add_char buffer '"';
        loop_string buffer (i + 1)
      )
    | _ -> assert false in
  let rec loop_token (l : int) (r : int) : unit =
    if l = n then
      ()
    else if r = n then
      Queue.add (Bytes.sub_string source l (r - l)) tokens
    else
      match Bytes.get source r with
      | '#' ->
        loop_comment (r + 1)
      | '"' ->
        (
          let buffer : Buffer.t = Buffer.create 32 in
          Buffer.add_char buffer '"';
          let l : int = loop_string buffer (r + 1) in
          Queue.add (Buffer.contents buffer) tokens;
          loop_token l l
        )
      | '\n' | '\t' | ' ' ->
        (
          if l <> r then (
            Queue.add (Bytes.sub_string source l (r - l)) tokens
          );
          let l : int = r + 1 in
          loop_token l l
        )
      | '(' | ')' | '{' | '}' | '=' | '+' | '-' ->
        (
          if l <> r then (
            Queue.add (Bytes.sub_string source l (r - l)) tokens
          );
          Queue.add (Bytes.sub_string source r 1) tokens;
          let l : int = r + 1 in
          loop_token l l
        )
      | _ -> loop_token l (r + 1)
  and loop_comment (i : int) : unit =
    if i = n then
      ()
    else
      match Bytes.get source i with
      | '\n' ->
        let i : int = i + 1 in
        loop_token i i
      | _ -> loop_comment (i + 1) in
  loop_token 0 0;
  Queue.to_seq tokens
  |> Seq.map into_token
  |> Queue.of_seq

let rec parse_args (tokens : token Queue.t) : string list =
  match Queue.peek tokens with
  | TokenIdent x ->
    let _ : token = Queue.pop tokens in
    x :: parse_args tokens
  | _ -> []

let parse_block (tokens : token Queue.t) (f : token Queue.t -> 'a) : 'a =
  match Queue.pop tokens with
  | TokenLBrace ->
    let x : 'a = f tokens in
    (match Queue.pop tokens with
     | TokenRBrace -> x
     | _ -> assert false)
  | _ -> assert false

let rec parse_call (tokens : token Queue.t) : expr =
  (match Queue.pop tokens with
   | TokenLParen -> ()
   | _ -> assert false);
  let expr : expr =
    match Queue.pop tokens with
    | TokenIdent x ->
      let args : expr list = parse_exprs tokens in
      let call : call =
        match x with
        | "printf" -> CallIntrin (IntrinPrintf)
        | "pack" -> CallIntrin (IntrinPack)
        | _ -> CallLabel x in
      ExprCall (call, args)
    | TokenEq ->
      (match parse_exprs tokens with
       | [l; r] -> ExprBinOp (BinOpEq, l, r)
       | _ -> assert false)
    | TokenAdd ->
      (match parse_exprs tokens with
       | [l; r] -> ExprBinOp (BinOpAdd, l, r)
       | _ -> assert false)
    | TokenSub ->
      (match parse_exprs tokens with
       | [l; r] -> ExprBinOp (BinOpSub, l, r)
       | _ -> assert false)
    | _ -> assert false in
  (match Queue.pop tokens with
   | TokenRParen -> ()
   | _ -> assert false);
  expr

and parse_let (tokens : token Queue.t) : expr =
  (match Queue.pop tokens with
   | TokenLet -> ()
   | _ -> assert false);
  let var : string =
    match Queue.pop tokens with
    | TokenIdent x -> x
    | _ -> assert false in
  let expr : expr =
    match parse_expr tokens with
    | Some expr -> expr
    | None -> assert false in
  ExprAssign (var, expr)

and parse_if (tokens : token Queue.t) : expr =
  (match Queue.pop tokens with
   | TokenIf -> ()
   | _ -> assert false);
  let condition : expr =
    match parse_expr tokens with
    | Some expr -> expr
    | None -> assert false in
  let exprs_then : expr list = parse_block tokens parse_exprs in
  (match Queue.pop tokens with
   | TokenElse -> ()
   | _ -> assert false);
  let exprs_else : expr list = parse_block tokens parse_exprs in
  ExprIfThen (condition, exprs_then, exprs_else)

and parse_branch (tokens : token Queue.t) : branch option =
  let args : string list = parse_args tokens in
  match Queue.peek tokens with
  | TokenLBrace ->
    let exprs : expr list = parse_block tokens parse_exprs in
    Some (args, exprs)
  | _ -> None

and parse_branches (tokens : token Queue.t) : branch list =
  match parse_branch tokens with
  | Some branch -> branch :: parse_branches tokens
  | None -> []

and parse_unpack (tokens : token Queue.t) : expr =
  (match Queue.pop tokens with
   | TokenUnpack -> ()
   | _ -> assert false);
  let packed : expr =
    match parse_expr tokens with
    | Some expr -> expr
    | None -> assert false in
  let branches : branch list = parse_block tokens parse_branches in
  ExprUnpack (packed, branches)

and parse_expr (tokens : token Queue.t) : expr option =
  match Queue.peek tokens with
  | TokenInt x ->
    let _ : token = Queue.pop tokens in
    Some (ExprInt x)
  | TokenIdent x ->
    let _ : token = Queue.pop tokens in
    Some (ExprVar x)
  | TokenStr x ->
    let _ : token = Queue.pop tokens in
    Some (ExprStr x)
  | TokenLParen -> Some (parse_call tokens)
  | TokenLet -> Some (parse_let tokens)
  | TokenIf -> Some (parse_if tokens)
  | TokenUnpack -> Some (parse_unpack tokens)
  | _ -> None

and parse_exprs (tokens : token Queue.t) : expr list =
  match parse_expr tokens with
  | None -> []
  | Some expr -> expr :: parse_exprs tokens

let parse_func (tokens : token Queue.t) : func =
  let label : string =
    match Queue.pop tokens with
    | TokenIdent x -> x
    | _ -> assert false in
  let args : string list = parse_args tokens in
  let body : expr list = parse_block tokens parse_exprs in
  {
    label;
    args;
    body;
  }

let rec parse (tokens : token Queue.t) : func Queue.t =
  let funcs : func Queue.t = Queue.create () in
  while (Queue.length tokens) <> 0 do
    Queue.add (parse_func tokens) funcs
  done;
  funcs

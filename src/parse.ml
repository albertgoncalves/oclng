open Types

type token =
  | TokenLParen
  | TokenRParen
  | TokenLBrace
  | TokenRBrace
  | TokenSlash

  | TokenReturn
  | TokenLet
  | TokenSwitch

  | TokenInt of int
  | TokenIdent of string
  | TokenStr of string

let show_token : token -> string =
  function
  | TokenLParen -> "("
  | TokenRParen -> ")"
  | TokenLBrace -> "{"
  | TokenRBrace -> "}"
  | TokenSlash -> "\\"

  | TokenReturn -> "return"
  | TokenLet -> "let"
  | TokenSwitch -> "switch"

  | TokenInt x -> string_of_int x
  | TokenIdent x -> x
  | TokenStr x -> Printf.sprintf "\"%s\"" x

type context =
  {
    mutable k : int;
    funcs : func Queue.t;
  }

let context : context =
  {
    k = 0;
    funcs = Queue.create ();
  }

let get_k () : int =
  let k : int = context.k in
  context.k <- context.k + 1;
  k

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
  | "\\" -> TokenSlash

  | "return" -> TokenReturn
  | "let" -> TokenLet
  | "switch" -> TokenSwitch

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
    | '\\' ->
      (
        Buffer.add_char buffer '\\';
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
      | '#' -> loop_comment (r + 1)
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
      | '(' | ')' | '{' | '}' | '\\' ->
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

let rec return_last (prev : stmt list) : stmt list -> stmt list =
  function
  | [] -> List.rev prev
  | [StmtHold (ExprSwitch (expr, branches))] ->
    return_last
      (
        StmtReturn (ExprSwitch (expr, List.map (return_last []) branches))
        :: prev
      )
      []
  | [StmtHold expr] -> return_last (StmtReturn expr :: prev) []
  | stmt :: stmts -> return_last (stmt :: prev) stmts

let rec parse_args
    (prev : string list)
    (tokens : token Queue.t) : string list =
  match Queue.peek tokens with
  | TokenIdent x ->
    let _ : token = Queue.pop tokens in
    parse_args (x :: prev) tokens
  | _ -> List.rev prev

let parse_block (tokens : token Queue.t) (f : token Queue.t -> 'a) : 'a =
  match Queue.pop tokens with
  | TokenLBrace ->
    let x : 'a = f tokens in
    (match Queue.pop tokens with
     | TokenRBrace -> x
     | _ -> assert false)
  | _ -> assert false

let rec parse_expr (tokens : token Queue.t) : expr option =
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
  | TokenSwitch -> Some (parse_switch tokens)
  | TokenSlash -> Some (parse_fn tokens)
  | _ -> None

and parse_exprs (prev : expr list) (tokens : token Queue.t) : expr list =
  match parse_expr tokens with
  | None -> List.rev prev
  | Some expr -> parse_exprs (expr :: prev) tokens

and parse_call (tokens : token Queue.t) : expr =
  (match Queue.pop tokens with
   | TokenLParen -> ()
   | _ -> assert false);
  let label : string =
    match Queue.pop tokens with
    | TokenIdent label -> label
    | _ -> assert false in
  let args : expr list = parse_exprs [] tokens in
  (match Queue.pop tokens with
   | TokenRParen -> ()
   | _ -> assert false);
  ExprCall (label, args)

and parse_branch
    (prev : stmt list list)
    (tokens : token Queue.t) : stmt list list =
  match Queue.peek tokens with
  | TokenLBrace ->
    parse_branch (parse_block tokens (parse_stmts []) :: prev) tokens
  | TokenRBrace ->
    let _ : token = Queue.pop tokens in
    List.rev prev
  | _ -> assert false

and parse_switch (tokens : token Queue.t) : expr =
  (match Queue.pop tokens with
   | TokenSwitch -> ()
   | _ -> assert false);
  let expr : expr =
    match parse_expr tokens with
    | Some expr -> expr
    | None -> assert false in
  (match Queue.pop tokens with
   | TokenLBrace -> ()
   | _ -> assert false);
  ExprSwitch (expr, parse_branch [] tokens)

and parse_fn (tokens : token Queue.t) : expr =
  (match Queue.pop tokens with
   | TokenSlash -> ()
   | _ -> assert false);
  let args : string list = parse_args [] tokens in
  let body : stmt list =
    return_last [] (parse_block tokens (parse_stmts [])) in
  let label : string = Printf.sprintf "_f%d_" (get_k ()) in
  Queue.add { label; args; body } context.funcs;
  ExprVar label

and parse_stmt (tokens : token Queue.t) : stmt option =
  match Queue.peek tokens with
  | TokenReturn ->
    let _ : token = Queue.pop tokens in
    (match parse_expr tokens with
     | Some expr -> Some (StmtReturn expr)
     | _ -> assert false)
  | TokenLet -> Some (parse_let tokens)
  | _ ->
    match parse_expr tokens with
    | Some expr -> Some (StmtDrop expr)
    | _ -> None

and parse_stmts (prev : stmt list) (tokens : token Queue.t) : stmt list =
  match parse_stmt tokens with
  | None ->
    (match prev with
     | StmtDrop expr :: rest -> List.rev (StmtHold expr :: rest)
     | (StmtReturn _ :: _) as stmts -> List.rev stmts
     | _ -> assert false)
  | Some stmt -> parse_stmts (stmt :: prev) tokens

and parse_let (tokens : token Queue.t) : stmt =
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
  StmtLet (var, expr)

let parse_func (tokens : token Queue.t) : func =
  let label : string =
    match Queue.pop tokens with
    | TokenIdent x -> x
    | _ -> assert false in
  let args : string list = parse_args [] tokens in
  let body : stmt list =
    return_last [] (parse_block tokens (parse_stmts [])) in
  {
    label;
    args;
    body;
  }

let parse (tokens : token Queue.t) : func Queue.t =
  while (Queue.length tokens) <> 0 do
    Queue.add (parse_func tokens) context.funcs
  done;
  context.funcs

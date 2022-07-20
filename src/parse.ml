open Types

type token =
  | TokenLParen
  | TokenRParen
  | TokenLBrace
  | TokenRBrace
  | TokenSemiC
  | TokenSlash

  | TokenReturn
  | TokenLet
  | TokenSwitch

  | TokenInt of int
  | TokenIdent of string
  | TokenStr of string

type token_pos = token * int

let show_token : token -> string =
  function
  | TokenLParen -> "("
  | TokenRParen -> ")"
  | TokenLBrace -> "{"
  | TokenRBrace -> "}"
  | TokenSemiC -> ";"
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
  }

let context : context =
  {
    k = 0;
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
  | ";" -> TokenSemiC
  | "\\" -> TokenSlash

  | "return" -> TokenReturn
  | "let" -> TokenLet
  | "switch" -> TokenSwitch

  | "entry" -> TokenIdent "_entry_"
  | "loop" -> TokenIdent "_loop_"

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
          assert (cs.[0] <> '_' || cs.[n - 1] <> '_' || cs = "_");
          TokenIdent cs
        )
    )

let tokenize () : token_pos Queue.t =
  let n : int = Bytes.length Io.context.source in
  let tokens : (string * int) Queue.t = Queue.create () in
  let rec loop_string (buffer : Buffer.t) (i : int) : int =
    match Bytes.get Io.context.source i with
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
    match Bytes.get Io.context.source i with
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
      Queue.add (Bytes.sub_string Io.context.source l (r - l), l) tokens
    else
      match Bytes.get Io.context.source r with
      | '#' -> loop_comment (r + 1)
      | '"' ->
        (
          let buffer : Buffer.t = Buffer.create 32 in
          Buffer.add_char buffer '"';
          let r : int = loop_string buffer (r + 1) in
          Queue.add (Buffer.contents buffer, l) tokens;
          loop_token r r
        )
      | '\n' | '\t' | ' ' ->
        (
          if l <> r then (
            Queue.add (Bytes.sub_string Io.context.source l (r - l), l) tokens
          );
          let r : int = r + 1 in
          loop_token r r
        )
      | '(' | ')' | '{' | '}' | ';' | '\\' ->
        (
          if l <> r then (
            Queue.add (Bytes.sub_string Io.context.source l (r - l), l) tokens
          );
          Queue.add (Bytes.sub_string Io.context.source r 1, r) tokens;
          let r : int = r + 1 in
          loop_token r r
        )
      | _ -> loop_token l (r + 1)
  and loop_comment (i : int) : unit =
    if i = n then
      ()
    else
      match Bytes.get Io.context.source i with
      | '\n' ->
        let i : int = i + 1 in
        loop_token i i
      | _ -> loop_comment (i + 1) in
  loop_token 0 0;
  let (tokens, offsets) : (string list * int list) =
    Queue.to_seq tokens
    |> List.of_seq
    |> List.split in
  let tokens : token list = List.map into_token tokens in
  List.combine tokens offsets
  |> List.to_seq
  |> Queue.of_seq

let peek (tokens : token_pos Queue.t) : token_pos =
  match Queue.peek_opt tokens with
  | Some token -> token
  | None -> Io.exit_at (Io.context.len - 1)

let pop (tokens : token_pos Queue.t) : token_pos =
  match Queue.take_opt tokens with
  | Some token -> token
  | None -> Io.exit_at (Io.context.len - 1)

let rec return_last (prev : stmt list) : stmt list -> stmt list =
  function
  | [] -> List.rev prev
  | StmtReturn (ExprSwitch (expr, branches)) :: rest ->
    return_last
      (
        StmtReturn (ExprSwitch (expr, List.map (return_last []) branches))
        :: prev
      )
      rest
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
    (tokens : token_pos Queue.t) : string list =
  match peek tokens with
  | (TokenIdent x, _) ->
    let _ : token_pos = pop tokens in
    parse_args (x :: prev) tokens
  | _ -> List.rev prev

let parse_block
    (tokens : token_pos Queue.t)
    (f : token_pos Queue.t -> 'a) : 'a =
  match pop tokens with
  | (TokenLBrace, _) ->
    let x : 'a = f tokens in
    (match pop tokens with
     | (TokenRBrace, _) -> x
     | (_, offset) -> Io.exit_at offset)
  | (_, offset) -> Io.exit_at offset

let rec resolve_expr (mapping : (string, string) Hashtbl.t) : expr -> expr =
  function
  | ExprVar var ->
    (match Hashtbl.find_opt mapping var with
     | Some label -> ExprVar label
     | None -> ExprVar var)
  | ExprFn func ->
    ExprFn
      {
        label = func.label;
        args = func.args;
        body = resolve_stmts (Hashtbl.copy mapping) func.body;
      }
  | ExprSwitch (expr, branches) ->
    ExprSwitch
      (
        resolve_expr mapping expr,
        List.map (resolve_stmts (Hashtbl.copy mapping)) branches
      )
  | ExprCall (expr, args) ->
    ExprCall (resolve_expr mapping expr, List.map (resolve_expr mapping) args)
  | expr -> expr

and resolve_stmts
    (mapping : (string, string) Hashtbl.t) : stmt list -> stmt list =
  function
  | [] -> []
  | StmtLet (var, (ExprFn func as expr)) :: rest ->
    (
      Hashtbl.add mapping var func.label;
      let rest : stmt list = resolve_stmts mapping rest in
      StmtDrop (resolve_expr mapping expr) :: rest
    )
  | StmtLet (var, expr) :: rest ->
    StmtLet (var, resolve_expr mapping expr) :: (resolve_stmts mapping rest)
  | StmtDrop expr :: rest ->
    StmtDrop (resolve_expr mapping expr) :: (resolve_stmts mapping rest)
  | StmtHold expr :: rest ->
    StmtHold (resolve_expr mapping expr) :: (resolve_stmts mapping rest)
  | StmtReturn expr :: rest ->
    StmtReturn (resolve_expr mapping expr) :: (resolve_stmts mapping rest)

let rec parse_expr (tokens : token_pos Queue.t) : expr option =
  match peek tokens with
  | (TokenInt x, _) ->
    let _ : token_pos = pop tokens in
    Some (ExprInt x)
  | (TokenIdent x, _) ->
    let _ : token_pos = pop tokens in
    Some (ExprVar x)
  | (TokenStr x, _) ->
    let _ : token_pos = pop tokens in
    Some (ExprStr x)
  | (TokenLParen, _) -> Some (parse_call tokens)
  | (TokenSwitch, _) -> Some (parse_switch tokens)
  | (TokenSlash, _) -> Some (parse_fn tokens)
  | _ -> None

and parse_exprs (prev : expr list) (tokens : token_pos Queue.t) : expr list =
  match parse_expr tokens with
  | None -> List.rev prev
  | Some expr -> parse_exprs (expr :: prev) tokens

and parse_call (tokens : token_pos Queue.t) : expr =
  (match pop tokens with
   | (TokenLParen, _) -> ()
   | (_, offset) -> Io.exit_at offset);
  let expr : expr =
    match parse_expr tokens with
    | Some expr -> expr
    | None -> assert false in
  let args : expr list = parse_exprs [] tokens in
  (match pop tokens with
   | (TokenRParen, _) -> ()
   | (_, offset) -> Io.exit_at offset);
  ExprCall (expr, args)

and parse_branch
    (prev : stmt list list)
    (tokens : token_pos Queue.t) : stmt list list =
  match peek tokens with
  | (TokenLBrace, _) ->
    parse_branch (parse_block tokens (parse_stmts []) :: prev) tokens
  | _ -> List.rev prev

and parse_switch (tokens : token_pos Queue.t) : expr =
  (match pop tokens with
   | (TokenSwitch, _) -> ()
   | (_, offset) -> Io.exit_at offset);
  let expr : expr =
    match parse_expr tokens with
    | Some expr -> expr
    | None -> assert false in
  let branches : stmt list list = parse_branch [] tokens in
  assert (List.length branches <> 0);
  ExprSwitch (expr, branches)

and parse_fn (tokens : token_pos Queue.t) : expr =
  (match pop tokens with
   | (TokenSlash, _) -> ()
   | (_, offset) -> Io.exit_at offset);
  let args : string list = parse_args [] tokens in
  let body : stmt list =
    return_last [] (parse_block tokens (parse_stmts [])) in
  let label : string = Printf.sprintf "_fn_%d_" (get_k ()) in
  ExprFn { label; args; body }

and parse_stmt (tokens : token_pos Queue.t) : stmt option =
  match peek tokens with
  | (TokenReturn, _) ->
    let _ : token_pos = pop tokens in
    (match parse_expr tokens with
     | Some expr -> Some (StmtReturn expr)
     | _ -> assert false)
  | (TokenLet, _) -> Some (parse_let tokens)
  | _ ->
    match parse_expr tokens with
    | Some expr -> Some (StmtDrop expr)
    | _ -> None

and parse_stmts (prev : stmt list) (tokens : token_pos Queue.t) : stmt list =
  match parse_stmt tokens with
  | None -> assert false
  | Some stmt ->
    match peek tokens with
    | (TokenSemiC, _) ->
      (
        let _ : token_pos = pop tokens in
        parse_stmts (stmt :: prev) tokens
      )
    | _ ->
      match stmt with
      | StmtDrop expr -> List.rev (StmtHold expr :: prev)
      | StmtReturn _ -> List.rev (stmt :: prev)
      | _ -> assert false

and parse_let (tokens : token_pos Queue.t) : stmt =
  (match pop tokens with
   | (TokenLet, _) -> ()
   | (_, offset) -> Io.exit_at offset);
  let var : string =
    match pop tokens with
    | (TokenIdent x, _) -> x
    | (_, offset) -> Io.exit_at offset in
  let expr : expr =
    match parse_expr tokens with
    | Some (ExprFn func) ->
      ExprFn
        {
          label = Printf.sprintf "_%s_%d_" var (get_k ());
          args = func.args;
          body = func.body
        }
    | Some expr -> expr
    | None -> assert false in
  StmtLet (var, expr)

let parse_func (tokens : token_pos Queue.t) : func =
  let label : string =
    match pop tokens with
    | (TokenIdent x, _) -> x
    | (_, offset) -> Io.exit_at offset in
  let args : string list = parse_args [] tokens in
  let body : stmt list =
    parse_stmts []
    |> parse_block tokens
    |> return_last []
    |> resolve_stmts (Hashtbl.create 16) in
  { label; args; body; }

let parse (tokens : token_pos Queue.t) : func Queue.t =
  let funcs : func Queue.t = Queue.create () in
  while (Queue.length tokens) <> 0 do
    Queue.add (parse_func tokens) funcs
  done;
  funcs

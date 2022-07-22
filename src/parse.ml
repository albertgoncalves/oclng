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

let peek (tokens : token_pos Queue.t) : token_pos =
  match Queue.peek_opt tokens with
  | Some token -> token
  | None -> Io.exit_at (Io.context.len - 1)

let pop (tokens : token_pos Queue.t) : token_pos =
  match Queue.take_opt tokens with
  | Some token -> token
  | None -> Io.exit_at (Io.context.len - 1)

let at_index (i : int) : char =
  if Io.context.len <= i then (
    Io.exit_at (Io.context.len - 1)
  );
  Bytes.get Io.context.source i

let is_digit : char -> bool =
  function
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
  | _ -> false

let is_space : char -> bool =
  function
  | '\n' | '\t' | ' ' -> true
  | _ -> false

let into_token : (string * int) -> token_pos =
  function
  | ("(", offset) -> (TokenLParen, offset)
  | (")", offset) -> (TokenRParen, offset)
  | ("{", offset) -> (TokenLBrace, offset)
  | ("}", offset) -> (TokenRBrace, offset)
  | (";", offset) -> (TokenSemiC, offset)
  | ("\\", offset) -> (TokenSlash, offset)

  | ("return", offset) -> (TokenReturn, offset)
  | ("let", offset) -> (TokenLet, offset)
  | ("switch", offset) -> (TokenSwitch, offset)

  | ("entry", offset) -> (TokenIdent "_entry_", offset)
  | ("loop", offset) -> (TokenIdent "_loop_", offset)

  | (cs, offset) ->
    (
      assert ((String.length cs) <> 0);
      if String.for_all is_digit cs then
        (TokenInt (int_of_string cs), offset)
      else
        let n : int = String.length cs in
        if (cs.[0] = '"') && (cs.[n - 1] = '"') then (
          (TokenStr (String.sub cs 1 (n - 2)), offset)
        ) else (
          assert (not (String.exists is_space cs));
          if not (cs.[0] <> '_' || cs.[n - 1] <> '_' || cs = "_") then (
            Io.exit_at offset
          );
          (TokenIdent cs, offset)
        )
    )

let tokenize () : token_pos Queue.t =
  let n : int = Bytes.length Io.context.source in
  let tokens : (string * int) Queue.t = Queue.create () in
  let rec loop_string (buffer : Buffer.t) (i : int) : int =
    match at_index i with
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
    match at_index i with
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
    | _ -> Io.exit_at i in
  let rec loop_token (l : int) (r : int) : unit =
    if l = n then
      ()
    else if r = n then
      Queue.add (Bytes.sub_string Io.context.source l (r - l), l) tokens
    else
      match at_index r with
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
      match at_index i with
      | '\n' ->
        let i : int = i + 1 in
        loop_token i i
      | _ -> loop_comment (i + 1) in
  loop_token 0 0;
  Queue.to_seq tokens
  |> Seq.map into_token
  |> Queue.of_seq

let rec return_last (prev : stmt_pos list) : stmt_pos list -> stmt_pos list =
  function
  | [] -> List.rev prev
  | (StmtReturn (ExprSwitch (expr, branches), o0), o1) :: rest ->
    return_last
      (
        (
          StmtReturn
            (ExprSwitch (expr, List.map (return_last []) branches), o0),
          o1
        ) :: prev
      )
      rest
  | [(StmtHold (ExprSwitch (expr, branches), o0), o1)] ->
    return_last
      (
        (
          StmtReturn
            (ExprSwitch (expr, List.map (return_last []) branches), o0),
          o1
        ) :: prev
      )
      []
  | [(StmtHold expr, offset)] ->
    return_last ((StmtReturn expr, offset) :: prev) []
  | stmt :: stmts -> return_last (stmt :: prev) stmts

let rec parse_args
    (prev : string_pos list)
    (tokens : token_pos Queue.t) : string_pos list =
  match peek tokens with
  | (TokenIdent x, offset) ->
    let _ : token_pos = pop tokens in
    parse_args ((x, offset) :: prev) tokens
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

let rec resolve_expr
    (mapping : (string, string) Hashtbl.t) : expr_pos -> expr_pos =
  function
  | (ExprVar var, offset) ->
    (match Hashtbl.find_opt mapping var with
     | Some label -> (ExprVar label, offset)
     | None -> (ExprVar var, offset))
  | (ExprFn func, offset) ->
    (
      ExprFn
        {
          label = func.label;
          args = func.args;
          body = resolve_stmts (Hashtbl.copy mapping) func.body;
        },
      offset
    )
  | (ExprSwitch (expr, branches), offset) ->
    (
      ExprSwitch
        (
          resolve_expr mapping expr,
          List.map (resolve_stmts (Hashtbl.copy mapping)) branches
        ),
      offset
    )
  | (ExprCall (expr, args), offset) ->
    (
      ExprCall
        (resolve_expr mapping expr, List.map (resolve_expr mapping) args),
      offset
    )
  | expr -> expr

and resolve_stmts
    (mapping : (string, string) Hashtbl.t) : stmt_pos list -> stmt_pos list =
  function
  | [] -> []
  | (StmtLet (var, ((ExprFn func, _) as expr)), offset) :: rest ->
    (
      Hashtbl.add mapping var (fst func.label);
      let rest : stmt_pos list = resolve_stmts mapping rest in
      (StmtDrop (resolve_expr mapping expr), offset) :: rest
    )
  | (StmtLet (var, expr), offset) :: rest ->
    (StmtLet (var, resolve_expr mapping expr), offset) ::
    (resolve_stmts mapping rest)
  | (StmtDrop expr, offset) :: rest ->
    (StmtDrop (resolve_expr mapping expr), offset) ::
    (resolve_stmts mapping rest)
  | (StmtHold expr, offset) :: rest ->
    (StmtHold (resolve_expr mapping expr), offset) ::
    (resolve_stmts mapping rest)
  | (StmtReturn expr, offset) :: rest ->
    (StmtReturn (resolve_expr mapping expr), offset) ::
    (resolve_stmts mapping rest)

let rec parse_expr (tokens : token_pos Queue.t) : (expr_pos, int) result =
  match peek tokens with
  | (TokenInt x, offset) ->
    let _ : token_pos = pop tokens in
    Ok (ExprInt x, offset)
  | (TokenIdent x, offset) ->
    let _ : token_pos = pop tokens in
    Ok (ExprVar x, offset)
  | (TokenStr x, offset) ->
    let _ : token_pos = pop tokens in
    Ok (ExprStr x, offset)
  | (TokenLParen, _) -> Ok (parse_call tokens)
  | (TokenSwitch, _) -> Ok (parse_switch tokens)
  | (TokenSlash, _) -> Ok (parse_fn tokens)
  | (_, offset) -> Error offset

and parse_exprs
    (prev : expr_pos list)
    (tokens : token_pos Queue.t) : expr_pos list =
  match parse_expr tokens with
  | Error _ -> List.rev prev
  | Ok expr -> parse_exprs (expr :: prev) tokens

and parse_call (tokens : token_pos Queue.t) : expr_pos =
  let offset : int =
    match pop tokens with
    | (TokenLParen, offset) -> offset
    | (_, offset) -> Io.exit_at offset in
  let expr : expr_pos =
    match parse_expr tokens with
    | Ok expr -> expr
    | Error offset -> Io.exit_at offset in
  let args : expr_pos list = parse_exprs [] tokens in
  (match pop tokens with
   | (TokenRParen, _) -> ()
   | (_, offset) -> Io.exit_at offset);
  (ExprCall (expr, args), offset)

and parse_branch
    (prev : stmt_pos list list)
    (tokens : token_pos Queue.t) : stmt_pos list list =
  match peek tokens with
  | (TokenLBrace, _) ->
    parse_branch (parse_block tokens (parse_stmts []) :: prev) tokens
  | _ -> List.rev prev

and parse_switch (tokens : token_pos Queue.t) : expr_pos =
  let o0 : int =
    match pop tokens with
    | (TokenSwitch, offset) -> offset
    | (_, offset) -> Io.exit_at offset in
  let expr : expr_pos =
    match parse_expr tokens with
    | Ok expr -> expr
    | Error offset -> Io.exit_at offset in
  let (_, o1) : token_pos = peek tokens in
  let branches : stmt_pos list list = parse_branch [] tokens in
  if List.length branches = 0 then (
    Io.exit_at o1
  );
  (ExprSwitch (expr, branches), o0)

and parse_fn (tokens : token_pos Queue.t) : expr_pos =
  let offset : int =
    match pop tokens with
    | (TokenSlash, offset) -> offset
    | (_, offset) -> Io.exit_at offset in
  let args : string_pos list = parse_args [] tokens in
  let body : stmt_pos list =
    return_last [] (parse_block tokens (parse_stmts [])) in
  let label : string = Printf.sprintf "_fn_%d_" (get_k ()) in
  (ExprFn { label = (label, offset); args; body }, offset)

and parse_stmt (tokens : token_pos Queue.t) : (stmt_pos, int) result =
  match peek tokens with
  | (TokenReturn, offset) ->
    let _ : token_pos = pop tokens in
    (match parse_expr tokens with
     | Ok expr -> Ok (StmtReturn expr, offset)
     | Error _ as error -> error)
  | (TokenLet, _) -> Ok (parse_let tokens)
  | (_, offset) ->
    match parse_expr tokens with
    | Ok expr -> Ok (StmtDrop expr, offset)
    | Error _ as error -> error

and parse_stmts
    (prev : stmt_pos list)
    (tokens : token_pos Queue.t) : stmt_pos list =
  match parse_stmt tokens with
  | Error offset -> Io.exit_at offset
  | Ok stmt ->
    match peek tokens with
    | (TokenSemiC, _) ->
      (
        let _ : token_pos = pop tokens in
        parse_stmts (stmt :: prev) tokens
      )
    | _ ->
      match stmt with
      | (StmtDrop expr, offset) -> List.rev ((StmtHold expr, offset) :: prev)
      | (StmtReturn _, _) -> List.rev (stmt :: prev)
      | (_, offset) -> Io.exit_at offset

and parse_let (tokens : token_pos Queue.t) : stmt_pos =
  let offset : int =
    match pop tokens with
    | (TokenLet, offset) -> offset
    | (_, offset) -> Io.exit_at offset in
  let var : string =
    match pop tokens with
    | (TokenIdent x, _) -> x
    | (_, offset) -> Io.exit_at offset in
  let expr : expr_pos =
    match parse_expr tokens with
    | Ok (ExprFn func, offset) ->
      (
        ExprFn
          {
            label = (Printf.sprintf "_%s_%d_" var (get_k ()), offset);
            args = func.args;
            body = func.body
          },
        offset
      )
    | Ok expr -> expr
    | Error offset -> Io.exit_at offset in
  (StmtLet (var, expr), offset)

let parse_func (tokens : token_pos Queue.t) : func =
  let (label, offset) : string_pos =
    match pop tokens with
    | (TokenIdent x, offset) -> (x, offset)
    | (_, offset) -> Io.exit_at offset in
  let args : string_pos list = parse_args [] tokens in
  let body : stmt_pos list =
    parse_stmts []
    |> parse_block tokens
    |> return_last []
    |> resolve_stmts (Hashtbl.create 16) in
  { label = (label, offset); args; body; }

let parse (tokens : token_pos Queue.t) : func Queue.t =
  let funcs : func Queue.t = Queue.create () in
  while (Queue.length tokens) <> 0 do
    Queue.add (parse_func tokens) funcs
  done;
  funcs

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
  | TokenSet
  | TokenSwitch

  | TokenInt of int
  | TokenIdent of string
  | TokenStr of string

type token_pos = token * Io.position

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
  | TokenSet -> "set"
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
  | None -> Io.exit_at (Io.position_at (Io.context.len - 1))

let pop (tokens : token_pos Queue.t) : token_pos =
  match Queue.take_opt tokens with
  | Some token -> token
  | None -> Io.exit_at (Io.position_at (Io.context.len - 1))

let at_index (i : int) : char =
  if Io.context.len <= i then (
    Io.exit_at (Io.position_at (Io.context.len - 1))
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

let into_token : (string * Io.position) -> token_pos =
  function
  | ("(", position) -> (TokenLParen, position)
  | (")", position) -> (TokenRParen, position)
  | ("{", position) -> (TokenLBrace, position)
  | ("}", position) -> (TokenRBrace, position)
  | (";", position) -> (TokenSemiC, position)
  | ("\\", position) -> (TokenSlash, position)

  | ("return", position) -> (TokenReturn, position)
  | ("let", position) -> (TokenLet, position)
  | ("set", position) -> (TokenSet, position)
  | ("switch", position) -> (TokenSwitch, position)

  | ("entry", position) -> (TokenIdent "_entry_", position)
  | ("loop", position) -> (TokenIdent "_loop_", position)

  | (cs, position) ->
    (
      assert ((String.length cs) <> 0);
      if String.for_all is_digit cs then
        (TokenInt (int_of_string cs), position)
      else
        let n : int = String.length cs in
        if (cs.[0] = '"') && (cs.[n - 1] = '"') then (
          (TokenStr (String.sub cs 1 (n - 2)), position)
        ) else (
          assert (not (String.exists is_space cs));
          if not (cs.[0] <> '_' || cs.[n - 1] <> '_' || cs = "_") then (
            Io.exit_at position
          );
          (TokenIdent cs, position)
        )
    )

let tokenize () : token_pos Queue.t =
  let n : int = Io.context.len in
  let tokens : string_pos Queue.t = Queue.create () in
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
    | _ -> Io.exit_at (Io.position_at i) in
  let rec loop_token (l : int) (r : int) : unit =
    if l = n then
      ()
    else if r = n then
      Queue.add
        (Bytes.sub_string Io.context.source l (r - l), Io.position_at l)
        tokens
    else
      match at_index r with
      | '#' -> loop_comment (r + 1)
      | '"' ->
        (
          let buffer : Buffer.t = Buffer.create 32 in
          Buffer.add_char buffer '"';
          let r : int = loop_string buffer (r + 1) in
          Queue.add (Buffer.contents buffer, Io.position_at l) tokens;
          loop_token r r
        )
      | '\n' | '\t' | ' ' ->
        (
          if l <> r then (
            Queue.add
              (Bytes.sub_string Io.context.source l (r - l), Io.position_at l)
              tokens
          );
          let r : int = r + 1 in
          loop_token r r
        )
      | '(' | ')' | '{' | '}' | ';' | '\\' ->
        (
          if l <> r then (
            Queue.add
              (Bytes.sub_string Io.context.source l (r - l), Io.position_at l)
              tokens
          );
          Queue.add
            (Bytes.sub_string Io.context.source r 1, Io.position_at r)
            tokens;
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
  | (StmtReturn (ExprSwitch (expr, branches), p0), p1) :: rest ->
    return_last
      (
        (
          StmtReturn
            (ExprSwitch (expr, List.map (return_last []) branches), p0),
          p1
        ) :: prev
      )
      rest
  | [(StmtHold (ExprSwitch (expr, branches), p0), p1)] ->
    return_last
      (
        (
          StmtReturn
            (ExprSwitch (expr, List.map (return_last []) branches), p0),
          p1
        ) :: prev
      )
      []
  | [(StmtHold expr, position)] ->
    return_last ((StmtReturn expr, position) :: prev) []
  | stmt :: stmts -> return_last (stmt :: prev) stmts

let rec parse_args
    (prev : string_pos list)
    (tokens : token_pos Queue.t) : string_pos list =
  match peek tokens with
  | (TokenIdent x, position) ->
    let _ : token_pos = pop tokens in
    parse_args ((x, position) :: prev) tokens
  | _ -> List.rev prev

let parse_block
    (tokens : token_pos Queue.t)
    (f : token_pos Queue.t -> 'a) : 'a =
  match pop tokens with
  | (TokenLBrace, _) ->
    let x : 'a = f tokens in
    (match pop tokens with
     | (TokenRBrace, _) -> x
     | (_, position) -> Io.exit_at position)
  | (_, position) -> Io.exit_at position

let rec resolve_expr
    (mapping : (string, string) Hashtbl.t) : expr_pos -> expr_pos =
  function
  | (ExprVar var, position) ->
    (match Hashtbl.find_opt mapping var with
     | Some label -> (ExprVar label, position)
     | None -> (ExprVar var, position))
  | (ExprFn func, position) ->
    (
      ExprFn
        {
          label = func.label;
          args = func.args;
          body = resolve_stmts (Hashtbl.copy mapping) func.body;
        },
      position
    )
  | (ExprSwitch (expr, branches), position) ->
    (
      ExprSwitch
        (
          resolve_expr mapping expr,
          List.map (resolve_stmts (Hashtbl.copy mapping)) branches
        ),
      position
    )
  | (ExprCall (expr, args), position) ->
    (
      ExprCall
        (resolve_expr mapping expr, List.map (resolve_expr mapping) args),
      position
    )
  | expr -> expr

and resolve_stmts
    (mapping : (string, string) Hashtbl.t) : stmt_pos list -> stmt_pos list =
  function
  | [] -> []
  | (StmtLet (var, ((ExprFn func, _) as expr)), position) :: rest ->
    (
      Hashtbl.add mapping var (fst func.label);
      let rest : stmt_pos list = resolve_stmts mapping rest in
      (StmtDrop (resolve_expr mapping expr), position) :: rest
    )
  | (StmtLet (var, expr), position) :: rest ->
    (StmtLet (var, resolve_expr mapping expr), position) ::
    (resolve_stmts mapping rest)
  | (StmtSet (var, offset, value), position) :: rest ->
    (
      StmtSet (resolve_expr mapping var, offset, resolve_expr mapping value),
      position
    ) :: (resolve_stmts mapping rest)
  | (StmtDrop expr, position) :: rest ->
    (StmtDrop (resolve_expr mapping expr), position) ::
    (resolve_stmts mapping rest)
  | (StmtHold expr, position) :: rest ->
    (StmtHold (resolve_expr mapping expr), position) ::
    (resolve_stmts mapping rest)
  | (StmtReturn expr, position) :: rest ->
    (StmtReturn (resolve_expr mapping expr), position) ::
    (resolve_stmts mapping rest)

let rec parse_expr
    (tokens : token_pos Queue.t) : (expr_pos, Io.position) result =
  match peek tokens with
  | (TokenInt x, position) ->
    let _ : token_pos = pop tokens in
    Ok (ExprInt x, position)
  | (TokenIdent x, position) ->
    let _ : token_pos = pop tokens in
    Ok (ExprVar x, position)
  | (TokenStr x, position) ->
    let _ : token_pos = pop tokens in
    Ok (ExprStr x, position)
  | (TokenLParen, _) -> Ok (parse_call tokens)
  | (TokenSwitch, _) -> Ok (parse_switch tokens)
  | (TokenSlash, _) -> Ok (parse_fn tokens)
  | (_, position) -> Error position

and parse_exprs
    (prev : expr_pos list)
    (tokens : token_pos Queue.t) : expr_pos list =
  match parse_expr tokens with
  | Error _ -> List.rev prev
  | Ok expr -> parse_exprs (expr :: prev) tokens

and parse_call (tokens : token_pos Queue.t) : expr_pos =
  let position : Io.position =
    match pop tokens with
    | (TokenLParen, position) -> position
    | (_, position) -> Io.exit_at position in
  let expr : expr_pos =
    match parse_expr tokens with
    | Ok expr -> expr
    | Error position -> Io.exit_at position in
  let args : expr_pos list = parse_exprs [] tokens in
  (match pop tokens with
   | (TokenRParen, _) -> ()
   | (_, position) -> Io.exit_at position);
  (ExprCall (expr, args), position)

and parse_branch
    (prev : stmt_pos list list)
    (tokens : token_pos Queue.t) : stmt_pos list list =
  match peek tokens with
  | (TokenLBrace, _) ->
    parse_branch (parse_block tokens (parse_stmts []) :: prev) tokens
  | _ -> List.rev prev

and parse_switch (tokens : token_pos Queue.t) : expr_pos =
  let p0 : Io.position =
    match pop tokens with
    | (TokenSwitch, position) -> position
    | (_, position) -> Io.exit_at position in
  let expr : expr_pos =
    match parse_expr tokens with
    | Ok expr -> expr
    | Error position -> Io.exit_at position in
  let (_, p1) : token_pos = peek tokens in
  let branches : stmt_pos list list = parse_branch [] tokens in
  if List.length branches = 0 then (
    Io.exit_at p1
  );
  (ExprSwitch (expr, branches), p0)

and parse_fn (tokens : token_pos Queue.t) : expr_pos =
  let position : Io.position =
    match pop tokens with
    | (TokenSlash, position) -> position
    | (_, position) -> Io.exit_at position in
  let args : string_pos list = parse_args [] tokens in
  let body : stmt_pos list =
    return_last [] (parse_block tokens (parse_stmts [])) in
  let label : string = Printf.sprintf "_fn_%d_" (get_k ()) in
  (ExprFn { label = (label, position); args; body; }, position)

and parse_stmt (tokens : token_pos Queue.t) : (stmt_pos, Io.position) result =
  match peek tokens with
  | (TokenReturn, position) ->
    let _ : token_pos = pop tokens in
    (match parse_expr tokens with
     | Ok expr -> Ok (StmtReturn expr, position)
     | Error _ as error -> error)
  | (TokenLet, _) -> Ok (parse_let tokens)
  | (TokenSet, _) -> Ok (parse_set tokens)
  | (_, position) ->
    match parse_expr tokens with
    | Ok expr -> Ok (StmtDrop expr, position)
    | Error _ as error -> error

and parse_stmts
    (prev : stmt_pos list)
    (tokens : token_pos Queue.t) : stmt_pos list =
  match parse_stmt tokens with
  | Error position -> Io.exit_at position
  | Ok stmt ->
    match peek tokens with
    | (TokenSemiC, _) ->
      (
        let _ : token_pos = pop tokens in
        parse_stmts (stmt :: prev) tokens
      )
    | _ ->
      match stmt with
      | (StmtDrop expr, position) ->
        List.rev ((StmtHold expr, position) :: prev)
      | (StmtReturn _, _) -> List.rev (stmt :: prev)
      | (_, position) -> Io.exit_at position

and parse_let (tokens : token_pos Queue.t) : stmt_pos =
  let position : Io.position =
    match pop tokens with
    | (TokenLet, position) -> position
    | (_, position) -> Io.exit_at position in
  let var : string =
    match pop tokens with
    | (TokenIdent x, _) -> x
    | (_, position) -> Io.exit_at position in
  let expr : expr_pos =
    match parse_expr tokens with
    | Ok (ExprFn func, position) ->
      (
        ExprFn
          {
            label = (Printf.sprintf "_%s_%d_" var (get_k ()), position);
            args = func.args;
            body = func.body;
          },
        position
      )
    | Ok expr -> expr
    | Error position -> Io.exit_at position in
  (StmtLet (var, expr), position)

and parse_set (tokens : token_pos Queue.t) : stmt_pos =
  let position : Io.position =
    match pop tokens with
    | (TokenSet, position) -> position
    | (_, position) -> Io.exit_at position in
  let var : expr_pos =
    match parse_expr tokens with
    | Ok var -> var
    | Error position -> Io.exit_at position in
  let offset : int =
    match pop tokens with
    | (TokenInt offset, _) -> offset
    | (_, position) -> Io.exit_at position in
  let value : expr_pos =
    match parse_expr tokens with
    | Ok value -> value
    | Error position -> Io.exit_at position in
  (StmtSet (var, offset, value), position)

let parse_func (tokens : token_pos Queue.t) : func =
  let (label, position) : string_pos =
    match pop tokens with
    | (TokenIdent x, position) -> (x, position)
    | (_, position) -> Io.exit_at position in
  let args : string_pos list = parse_args [] tokens in
  let body : stmt_pos list =
    parse_stmts []
    |> parse_block tokens
    |> return_last []
    |> resolve_stmts (Hashtbl.create 16) in
  { label = (label, position); args; body; }

let parse (tokens : token_pos Queue.t) : func Queue.t =
  let funcs : func Queue.t = Queue.create () in
  while (Queue.length tokens) <> 0 do
    Queue.add (parse_func tokens) funcs
  done;
  funcs

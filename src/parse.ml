type stmt =
  | StmtDrop of expr_pos
  | StmtHold of expr_pos
  | StmtReturn of expr_pos
  | StmtLet of (string * expr_pos)
  | StmtSetLocal of (string * expr_pos)
  | StmtSetHeap of (expr_pos * int * expr_pos)
  | StmtNew of (string * string * int * expr_pos list)
  | StmtInto of (expr_pos * int * int * expr_pos list)

and expr =
  | ExprInt of int
  | ExprIndex of (int * int * int)
  | ExprStr of string
  | ExprVar of string
  | ExprFunc of func
  | ExprCall of (expr_pos * expr_pos list)
  | ExprSwitch of (expr_pos * stmt_pos list list)

and type' =
  | TypeAny
  | TypeVar of string
  | TypeRange of (int * int)
  | TypeInt
  | TypeStr
  | TypeFunc of ((type' list) * type')
  | TypeHeap of type' list
  | TypeGeneric of string
  | TypeStruct of string

and func =
  {
    label : string_pos;
    args : string_type_pos list;
    body : stmt_pos list;
  }

and stmt_pos = (stmt * Io.position)

and expr_pos = (expr * Io.position)

and string_pos = (string * Io.position)

and string_type_pos = (string * (type' option) * Io.position)

and type_pos = (type' * (Io.position option))

type structs = (string, type_pos) Hashtbl.t

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
  | ExprIndex (x, l, r) -> Printf.sprintf "%d [%d, %d]" x l r
  | ExprStr "" -> "\"\""
  | ExprStr str -> show_string str
  | ExprVar var -> var
  | ExprFunc func -> Printf.sprintf "%s { ... }" (fst func.label)
  | ExprCall (expr, []) -> Printf.sprintf "(%s)" (show_expr_pos expr)
  | ExprCall (expr, args) ->
    Printf.sprintf "(%s %s)" (show_expr_pos expr) (show_exprs args)
  | ExprSwitch (expr, branches) ->
    Printf.sprintf
      "switch %s { %s }"
      (show_expr_pos expr)
      (
        branches
        |> List.map
          (fun s ->
             String.concat "; " (s |> List.map fst |> List.map show_stmt))
        |> String.concat " } { "
      )

and show_exprs (exprs : expr_pos list) : string =
  exprs
  |> List.map fst
  |> List.map show_expr
  |> String.concat " "

and show_expr_pos ((expr, _) : expr_pos) : string =
  show_expr expr

and show_stmt : stmt -> string =
  function
  | StmtDrop expr -> Printf.sprintf "drop %s" (show_expr_pos expr)
  | StmtHold expr -> Printf.sprintf "hold %s" (show_expr_pos expr)
  | StmtReturn expr -> Printf.sprintf "return %s" (show_expr_pos expr)
  | StmtLet (var, expr) -> Printf.sprintf "let %s %s" var (show_expr_pos expr)
  | StmtSetLocal (var, expr) ->
    Printf.sprintf "set %s %s" var (show_expr_pos expr)
  | StmtSetHeap (var, offset, value) ->
    Printf.sprintf
      "seta %s %d %s"
      (show_expr_pos var)
      offset
      (show_expr_pos value)
  | StmtNew (var, struct', index, exprs) ->
    Printf.sprintf "new %s %s %d %s" var struct' index (show_exprs exprs)
  | StmtInto (expr, from, to', exprs) ->
    Printf.sprintf
      "into %s %d %d %s"
      (show_expr (fst expr))
      from
      to'
      (show_exprs exprs)

let show_func (func : func) : string =
  let (label, _) : string * Io.position = func.label in
  let body : string =
    String.concat
      ";\n    "
      (List.map (fun s -> s |> fst |> show_stmt) func.body) in
  if (List.length func.args) = 0 then
    Printf.sprintf "%s {\n    %s\n}\n" label body
  else
    Printf.sprintf
      "%s %s {\n    %s\n}\n"
      label
      (String.concat " " (List.map (fun (ident, _, _) -> ident) func.args))
      body

let rec show_type : type' -> string =
  function
  | TypeAny -> "any"
  | TypeVar var -> var
  | TypeRange (l, r) -> Printf.sprintf "[%d, %d]" l r
  | TypeInt -> "int"
  | TypeStr -> "str"
  | TypeFunc (args, return) ->
    Printf.sprintf "\\%s { %s }" (show_types args) (show_type return)
  | TypeHeap items -> Printf.sprintf "(%s)" (show_types items)
  | TypeGeneric var -> Printf.sprintf "'%s" var
  | TypeStruct struct' -> struct'

and show_types (types : type' list) : string =
  String.concat " " (List.map show_type types)

type token =
  | TokenLParen
  | TokenRParen
  | TokenLBrace
  | TokenRBrace
  | TokenLBracket
  | TokenRBracket
  | TokenSemiC
  | TokenSlash

  | TokenAt
  | TokenUnderS

  | TokenReturn
  | TokenLet
  | TokenSet
  | TokenSetA
  | TokenSwitch

  | TokenNew
  | TokenInto

  | TokenInt of int
  | TokenIdent of string
  | TokenStr of string

  | TokenTypeInt
  | TokenTypeStr

type token_pos = token * Io.position

let show_token : token -> string =
  function
  | TokenLParen -> "("
  | TokenRParen -> ")"
  | TokenLBrace -> "{"
  | TokenRBrace -> "}"
  | TokenLBracket -> "["
  | TokenRBracket -> "]"
  | TokenSemiC -> ";"
  | TokenSlash -> "\\"

  | TokenAt -> "@"
  | TokenUnderS -> "_"

  | TokenReturn -> "return"
  | TokenLet -> "let"
  | TokenSet -> "set"
  | TokenSetA -> "seta"
  | TokenSwitch -> "switch"

  | TokenNew -> "new"
  | TokenInto -> "into"

  | TokenInt x -> string_of_int x
  | TokenIdent ident -> ident
  | TokenStr str -> Printf.sprintf "\"%s\"" str

  | TokenTypeInt -> "Int"
  | TokenTypeStr -> "Str"

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
  | None ->
    Io.exit_at
      (Io.position_at (Io.context.len - 1))
      "reached end of file, no tokens remaining"

let pop (tokens : token_pos Queue.t) : token_pos =
  match Queue.take_opt tokens with
  | Some token -> token
  | None ->
    Io.exit_at
      (Io.position_at (Io.context.len - 1))
      "reached end of file, no tokens remaining"

let at_index (i : int) : char =
  if Io.context.len <= i then (
    Io.exit_at
      (Io.position_at (Io.context.len - 1))
      "reached end of file, no characters remaining"
  );
  Bytes.get Io.context.source i

let is_digit (c : char) : bool =
  ('0' <= c) && (c <= '9')

let is_lower (c : char) : bool =
  ('a' <= c) && (c <= 'z')

let is_upper (c : char) : bool =
  ('A' <= c) && (c <= 'Z')

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
  | ("[", position) -> (TokenLBracket, position)
  | ("]", position) -> (TokenRBracket, position)
  | (";", position) -> (TokenSemiC, position)
  | ("\\", position) -> (TokenSlash, position)
  | ("@", position) -> (TokenAt, position)
  | ("_", position) -> (TokenUnderS, position)

  | ("return", position) -> (TokenReturn, position)
  | ("let", position) -> (TokenLet, position)
  | ("set", position) -> (TokenSet, position)
  | ("seta", position) -> (TokenSetA, position)
  | ("switch", position) -> (TokenSwitch, position)

  | ("new", position) -> (TokenNew, position)
  | ("into", position) -> (TokenInto, position)

  | ("entry", position) -> (TokenIdent "entry_", position)
  | ("loop", position) -> (TokenIdent "loop_", position)

  | ("Int", position) -> (TokenTypeInt, position)
  | ("Str", position) -> (TokenTypeStr, position)

  | (ident, position) ->
    (
      assert ((String.length ident) <> 0);
      if String.for_all is_digit ident then
        (TokenInt (int_of_string ident), position)
      else
        let n : int = String.length ident in
        if (ident.[0] = '"') && (ident.[n - 1] = '"') then (
          (TokenStr (String.sub ident 1 (n - 2)), position)
        ) else (
          assert (not (String.exists is_space ident));
          if not (ident.[0] <> '_' || ident.[n - 1] <> '_' || ident = "_")
          then (
            Io.exit_at position (Printf.sprintf "invalid string \"%s\"" ident)
          );
          (TokenIdent ident, position)
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
    | c ->
      Io.exit_at
        (Io.position_at i)
        (Printf.sprintf "unexpected character '%c'" c) in
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
      | '(' | ')' | '{' | '}' | '[' | ']' | ';' | '\\' | '@' ->
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

let exit_unexpected_token ((token, position) : token_pos) : 'a =
  Io.exit_at
    position
    (Printf.sprintf "unexpected token `%s`" (show_token token))

let rec parse_type (tokens : token_pos Queue.t) : type_pos option =
  match peek tokens with
  | (TokenTypeInt, position) ->
    let _ : token_pos = pop tokens in
    Some (TypeInt, Some position)
  | (TokenTypeStr, position) ->
    let _ : token_pos = pop tokens in
    Some (TypeStr, Some position)
  | (TokenLParen, position) ->
    let _ : token_pos = pop tokens in
    let items : type' list = List.map fst (parse_types [] tokens) in
    (match pop tokens with
     | (TokenRParen, _) -> Some (TypeHeap items, Some position)
     | token -> exit_unexpected_token token);
  | (TokenSlash, position) ->
    let _ : token_pos = pop tokens in
    let args : type' list = List.map fst (parse_types [] tokens) in
    (match pop tokens with
     | (TokenLBrace, _)  -> ()
     | token -> exit_unexpected_token token);
    let return : type' =
      match parse_type tokens with
      | Some (type', _) -> type'
      | None -> Io.exit_at position "expected return type" in
    (match pop tokens with
     | (TokenRBrace, _)  -> ()
     | token -> exit_unexpected_token token);
    Some (TypeFunc (args, return), Some position)
  | (TokenIdent ident, position) as token when is_upper ident.[0] ->
    (
      if not (is_upper ident.[0]) then (
        exit_unexpected_token token
      );
      let _ : token_pos = pop tokens in
      Some (TypeStruct ident, Some position)
    )
  | _ -> None

and parse_types
    (prev : type_pos list)
    (tokens : token_pos Queue.t) : type_pos list =
  match parse_type tokens with
  | None -> List.rev prev
  | Some type' -> parse_types (type' :: prev) tokens

let rec parse_args
    (prev : string_type_pos list)
    (tokens : token_pos Queue.t) : string_type_pos list =
  match peek tokens with
  | (TokenIdent arg, position) ->
    let _ : token_pos = pop tokens in
    (match peek tokens with
     | (TokenAt, _) ->
       (
         let _ : token_pos = pop tokens in
         match parse_type tokens with
         | None -> assert false
         | Some (type', _) ->
           parse_args ((arg, Some type', position) :: prev) tokens
       )
     | _ -> parse_args ((arg, None, position) :: prev) tokens)
  | _ -> List.rev prev

let parse_block
    (tokens : token_pos Queue.t)
    (f : token_pos Queue.t -> 'a) : 'a =
  match pop tokens with
  | (TokenLBrace, _) ->
    let x : 'a = f tokens in
    (match pop tokens with
     | (TokenRBrace, _) -> x
     | token -> exit_unexpected_token token)
  | token -> exit_unexpected_token token

let rec resolve_expr
    (mapping : (string, string) Hashtbl.t) : expr_pos -> expr_pos =
  function
  | (ExprVar var, position) ->
    (match Hashtbl.find_opt mapping var with
     | Some label -> (ExprVar label, position)
     | None -> (ExprVar var, position))
  | (ExprFunc func, position) ->
    (
      Printf.fprintf stderr "%s\n" (show_func func);
      ExprFunc
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
  | (StmtLet (var, ((ExprFunc func, _) as expr)), position) :: rest ->
    (
      Hashtbl.add mapping var (fst func.label);
      (StmtDrop (resolve_expr mapping expr), position) ::
      (resolve_stmts mapping rest)
    )
  | (StmtLet (var, expr), position) :: rest ->
    (StmtLet (var, resolve_expr mapping expr), position) ::
    (resolve_stmts mapping rest)
  | (StmtSetLocal (var, value), position) :: rest ->
    (StmtSetLocal (var, resolve_expr mapping value), position) ::
    (resolve_stmts mapping rest)
  | (StmtSetHeap (var, offset, value), position) :: rest ->
    (
      StmtSetHeap
        (resolve_expr mapping var, offset, resolve_expr mapping value),
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
  | (StmtNew (var, struct', index, exprs), position) :: rest ->
    (
      StmtNew (var, struct', index, List.map (resolve_expr mapping) exprs),
      position
    ) :: (resolve_stmts mapping rest)
  | (StmtInto (expr, from, to', exprs), position) :: rest ->
    (
      (
        StmtInto
          (
            resolve_expr mapping expr,
            from,
            to',
            List.map (resolve_expr mapping) exprs
          ),
        position
      )
    ) :: (resolve_stmts mapping rest)

let rec parse_expr
    (tokens : token_pos Queue.t) : (expr_pos, Io.position) result =
  match peek tokens with
  | (TokenInt x, position) ->
    let _ : token_pos = pop tokens in
    Ok (ExprInt x, position)
  | (TokenIdent ident, position) ->
    let _ : token_pos = pop tokens in
    Ok (ExprVar ident, position)
  | (TokenStr str, position) ->
    let _ : token_pos = pop tokens in
    Ok (ExprStr str, position)
  | (TokenLParen, _) -> Ok (parse_call tokens)
  | (TokenSwitch, _) -> Ok (parse_switch tokens)
  | (TokenSlash, _) -> Ok (parse_func tokens)
  | (TokenLBracket, _) -> Ok (parse_range tokens)
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
    | token -> exit_unexpected_token token in
  let expr : expr_pos =
    match parse_expr tokens with
    | Ok expr -> expr
    | Error position ->
      Io.exit_at
        position
        "failed to parse expression while parsing function call" in
  let args : expr_pos list = parse_exprs [] tokens in
  (match pop tokens with
   | (TokenRParen, _) -> ()
   | token -> exit_unexpected_token token);
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
    | token -> exit_unexpected_token token in
  let expr : expr_pos =
    match parse_expr tokens with
    | Ok expr -> expr
    | Error position ->
      Io.exit_at
        position
        "failed to parse expression while parsing `switch` statement" in
  let (_, p1) : token_pos = peek tokens in
  let branches : stmt_pos list list = parse_branch [] tokens in
  if List.length branches = 0 then (
    Io.exit_at p1 "`switch` statements require at least one branch"
  );
  (ExprSwitch (expr, branches), p0)

and parse_func (tokens : token_pos Queue.t) : expr_pos =
  let position : Io.position =
    match pop tokens with
    | (TokenSlash, position) -> position
    | token -> exit_unexpected_token token in
  let args : string_type_pos list = parse_args [] tokens in
  let body : stmt_pos list =
    return_last [] (parse_block tokens (parse_stmts [])) in
  let label : string = Printf.sprintf "_fn_%d_" (get_k ()) in
  (ExprFunc { label = (label, position); args; body; }, position)

and parse_range (tokens : token_pos Queue.t) : expr_pos =
  let position : Io.position =
    match pop tokens with
    | (TokenLBracket, position) -> position
    | token -> exit_unexpected_token token in
  let (x, p0) : int * Io.position =
    match pop tokens with
    | (TokenInt x, p0) -> (x, p0)
    | token -> exit_unexpected_token token in
  let l : int =
    match pop tokens with
    | (TokenInt l, _) -> l
    | token -> exit_unexpected_token token in
  let r : int =
    match pop tokens with
    | (TokenInt r, _) -> r
    | token -> exit_unexpected_token token in
  if (x < l) || (r < x) then (
    Io.exit_at
      position
      (Printf.sprintf "%d not contained within range [%d, %d]" x l r)
  );
  (match pop tokens with
   | (TokenRBracket, _) -> ()
   | token -> exit_unexpected_token token);
  (ExprIndex (x, l, r), position)

and parse_stmt (tokens : token_pos Queue.t) : (stmt_pos, Io.position) result =
  match peek tokens with
  | (TokenReturn, position) ->
    let _ : token_pos = pop tokens in
    (match parse_expr tokens with
     | Ok expr -> Ok (StmtReturn expr, position)
     | Error _ as error -> error)
  | (TokenLet, _) -> Ok (parse_let tokens)
  | (TokenSet, _) -> Ok (parse_set_local tokens)
  | (TokenSetA, _) -> Ok (parse_set_heap tokens)
  | (TokenNew, _) -> Ok (parse_new tokens)
  | (TokenInto, _) -> Ok (parse_into tokens)
  | (_, position) ->
    match parse_expr tokens with
    | Ok expr -> Ok (StmtDrop expr, position)
    | Error _ as error -> error

and parse_stmts
    (prev : stmt_pos list)
    (tokens : token_pos Queue.t) : stmt_pos list =
  match parse_stmt tokens with
  | Error position -> Io.exit_at position "failed to parse statement"
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
      | (_, position) ->
        Io.exit_at
          position
          "blocks must end with a value or a `return` statement"

and parse_let (tokens : token_pos Queue.t) : stmt_pos =
  let position : Io.position =
    match pop tokens with
    | (TokenLet, position) -> position
    | _ -> assert false in
  let var : string =
    match pop tokens with
    | (TokenIdent ident, _) -> ident
    | token -> exit_unexpected_token token in
  let expr : expr_pos =
    match parse_expr tokens with
    | Ok (ExprFunc func, position) ->
      (
        ExprFunc
          {
            label = (Printf.sprintf "_%s_%d_" var (get_k ()), position);
            args = func.args;
            body = func.body;
          },
        position
      )
    | Ok expr -> expr
    | Error position ->
      Io.exit_at
        position
        "failed to parse expression while parsing `let` statement" in
  (StmtLet (var, expr), position)

and parse_set_local (tokens : token_pos Queue.t) : stmt_pos =
  let position : Io.position =
    match pop tokens with
    | (TokenSet, position) -> position
    | _ -> assert false in
  let var : string =
    match pop tokens with
    | (TokenIdent ident, _) -> ident
    | token -> exit_unexpected_token token in
  let expr : expr_pos =
    match parse_expr tokens with
    | Ok (ExprFunc func, position) ->
      (
        ExprFunc
          {
            label = (Printf.sprintf "_%s_%d_" var (get_k ()), position);
            args = func.args;
            body = func.body;
          },
        position
      )
    | Ok expr -> expr
    | Error position ->
      Io.exit_at
        position
        "failed to parse expression while parsing `set` statement" in
  (StmtSetLocal (var, expr), position)

and parse_set_heap (tokens : token_pos Queue.t) : stmt_pos =
  let position : Io.position =
    match pop tokens with
    | (TokenSetA, position) -> position
    | _ -> assert false in
  let var : expr_pos =
    match parse_expr tokens with
    | Ok var -> var
    | Error position ->
      Io.exit_at
        position
        "failed to parse expression while parsing `seta` statement" in
  let offset : int =
    match pop tokens with
    | (TokenInt offset, _) -> offset
    | token -> exit_unexpected_token token in
  let value : expr_pos =
    match parse_expr tokens with
    | Ok value -> value
    | Error position ->
      Io.exit_at
        position
        "failed to parse expression while parsing `seta` statement" in
  (StmtSetHeap (var, offset, value), position)

and parse_new (tokens : token_pos Queue.t) : stmt_pos =
  let position : Io.position =
    match pop tokens with
    | (TokenNew, position) -> position
    | _ -> assert false in
  let var : string =
    match pop tokens with
    | (TokenIdent var, _) -> var
    | token -> exit_unexpected_token token in
  let struct' : string =
    match pop tokens with
    | (TokenIdent struct', _) -> struct'
    | token -> exit_unexpected_token token in
  let index : int =
    match pop tokens with
    | (TokenInt index, _) -> index
    | token -> exit_unexpected_token token in
  (StmtNew (var, struct', index, parse_exprs [] tokens), position)

and parse_into (tokens : token_pos Queue.t) : stmt_pos =
  let position : Io.position =
    match pop tokens with
    | (TokenInto, position) -> position
    | _ -> assert false in
  let expr : expr_pos =
    match parse_expr tokens with
    | Ok value -> value
    | Error position ->
      Io.exit_at
        position
        "failed to parse expression while parsing `into` statement" in
  let from : int =
    match pop tokens with
    | (TokenInt from, _) -> from
    | token -> exit_unexpected_token token in
  let to': int =
    match pop tokens with
    | (TokenInt to', _) -> to'
    | token -> exit_unexpected_token token in
  (StmtInto (expr, from, to', parse_exprs [] tokens), position)

let parse_top_func (tokens : token_pos Queue.t) : func =
  let (label, position) : string_pos =
    match pop tokens with
    | (TokenIdent ident, position) -> (ident, position)
    | token -> exit_unexpected_token token in
  let args : string_type_pos list = parse_args [] tokens in
  let body : stmt_pos list =
    parse_stmts []
    |> parse_block tokens
    |> return_last []
    |> resolve_stmts (Hashtbl.create 16) in
  let func : func = { label = (label, position); args; body; } in
  Printf.fprintf stderr "%s\n" (show_func func);
  func

let parse (tokens : token_pos Queue.t) : (func Queue.t * structs) =
  let funcs : func Queue.t = Queue.create () in
  let structs : structs = Hashtbl.create 16 in
  while (Queue.length tokens) <> 0 do
    match peek tokens with
    | (TokenIdent ident, _) when is_lower ident.[0] ->
      Queue.add (parse_top_func tokens) funcs
    | (TokenIdent ident, position) ->
      (match Hashtbl.find_opt structs ident with
       | None ->
         let _ : token_pos = pop tokens in
         (match parse_type tokens with
          | Some type' -> Hashtbl.add structs ident type'
          | None -> assert false)
       | Some _ ->
         Io.exit_at
           position
           (Printf.sprintf "struct `%s` already defined" ident))
    | token -> exit_unexpected_token token
  done;
  (funcs, structs)

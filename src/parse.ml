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
  | TokenI64 of int
  | TokenIdent of string
  | TokenStr of string

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
        TokenI64 (int_of_string cs)
      else
        let n : int = String.length cs in
        if (cs.[0] = '"') && (cs.[n - 1] = '"') then (
          TokenStr (String.sub cs 1 (n - 2))
        ) else (
          assert (String.for_all (fun x -> not (is_space x)) cs);
          TokenIdent cs
        )
    )

let tokenize (source : bytes) : token list =
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
      | _ -> loop_token l (r + 1) in
  loop_token 0 0;
  Queue.to_seq tokens
  |> Seq.map into_token
  |> List.of_seq

let parse (tokens : token list) : func list =
  []

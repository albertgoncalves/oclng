type context =
  {
    mutable path : string;
    mutable source : Bytes.t;
    mutable len : int;
  }

type position =
  {
    path : string;
    source : Bytes.t;
    offset : int;
  }

let context : context =
  {
    path = "";
    source = Bytes.empty;
    len = 0;
  }

let load_file (path : string) : unit =
  context.path <- path;
  let file : in_channel = open_in context.path in
  context.len <- in_channel_length file;
  context.source <- Bytes.create context.len;
  really_input file context.source 0 context.len;
  close_in file

let position_at (offset : int) : position =
  { path = context.path; source = context.source; offset; }

let exit_at (position : position) : 'a =
  let rec loop ((row, col) : int * int) (i : int) : (int * int) =
    if i = position.offset then
      (row, col)
    else
      let (row, col) : (int * int) =
        match Bytes.get position.source i with
        | '\n' -> (row + 1, 1)
        | _ -> (row, col + 1) in
      loop (row, col) (i + 1) in
  let (row, col) : (int * int) = loop (1, 1) 0 in
  Printf.fprintf stderr "%s:%d:%d\n" position.path row col;
  exit 1

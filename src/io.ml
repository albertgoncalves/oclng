type context =
  {
    mutable path : string;
    mutable source : Bytes.t;
  }

let context : context =
  {
    path = "";
    source = Bytes.empty;
  }

let load_file (path : string) : unit =
  context.path <- path;
  let file : in_channel = open_in context.path in
  let n : int = in_channel_length file in
  context.source <- Bytes.create n;
  really_input file context.source 0 n;
  close_in file

let exit_at (offset : int) : 'a =
  let rec loop ((row, col) : int * int) (i : int) : (int * int) =
    if i = offset then
      (row, col)
    else
      let (row, col) : (int * int) =
        match Bytes.get context.source i with
        | '\n' -> (row + 1, 1)
        | _ -> (row, col + 1) in
      loop (row, col) (i + 1) in
  let (row, col) : (int * int) = loop (1, 1) 0 in
  Printf.fprintf stderr "%s:%d:%d\n" context.path row col;
  exit 1

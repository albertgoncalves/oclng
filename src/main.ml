let () : unit =
  Io.load_file Sys.argv.(1);
  let (funcs, structs) : (Parse.func Queue.t * Parse.structs) =
    Parse.parse (Parse.tokenize ()) in
  Check.check funcs structs;
  let buffer : Buffer.t = Compile.compile funcs in
  let file : out_channel = open_out Sys.argv.(2) in
  Buffer.output_buffer file buffer;
  close_out file

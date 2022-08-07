let () : unit =
  Io.load_file Sys.argv.(1);
  let funcs : Parse.func Queue.t = Parse.parse (Parse.tokenize ()) in
  Check.check funcs;
  let buffer : Buffer.t = Compile.compile funcs in
  let file : out_channel = open_out Sys.argv.(2) in
  Buffer.output_buffer file buffer;
  close_out file

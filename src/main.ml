let () : unit =
  Io.load_file Sys.argv.(1);
  let buffer : Buffer.t =
    Parse.tokenize ()
    |> Parse.parse
    |> Compile.compile in
  let file : out_channel = open_out Sys.argv.(2) in
  Buffer.output_buffer file buffer;
  close_out file

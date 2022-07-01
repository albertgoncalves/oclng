open Compile
open Parse
open Types

let () : unit =
  let file : in_channel = open_in Sys.argv.(1) in
  let n : int = in_channel_length file in
  let source : bytes = Bytes.create n in
  really_input file source 0 n;
  close_in file;
  let buffer : Buffer.t =
    tokenize source
    |> parse
    |> Queue.to_seq
    |> List.of_seq
    |> compile in
  let file : out_channel = open_out Sys.argv.(2) in
  Buffer.output_buffer file buffer;
  close_out file

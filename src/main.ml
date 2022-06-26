type context =
  {
    mutable k : int;
    strings : (string, string) Hashtbl.t;
  }

type reg =
  | RegRdi
  | RegRsi
  | RegRdx
  | RegRcx
  | RegR8
  | RegR9

type intrin =
  | IntrinPrintf

type expr =
  | ExprDrop of expr
  | ExprRet of expr
  | ExprIntrin of (intrin * expr list)
  | ExprStr of string

type func =
  {
    label : string;
    args : string list;
    body : expr list;
  }

let context : context =
  {
    k = 0;
    strings = Hashtbl.create 64;
  }

let buffer : Buffer.t = Buffer.create 1024

let append_buffer : string -> unit = Buffer.add_string buffer

let arg_regs : reg list = [RegRdi; RegRsi; RegRdx; RegRcx; RegR8; RegR9]

let get_k () : int =
  let k : int = context.k in
  context.k <- context.k + 1;
  k

let show_reg : reg -> string =
  function
  | RegRdi -> "rdi"
  | RegRsi -> "rsi"
  | RegRdx -> "rdx"
  | RegRcx -> "rcx"
  | RegR8 -> "r8"
  | RegR9 -> "r9"

let compile_string (str : string) (label : string) : unit =
  String.to_seq str
  |> Seq.map Char.code
  |> Seq.map string_of_int
  |> List.of_seq
  |> String.concat ","
  |> Printf.sprintf "\t%s db %s,0\n" label
  |> append_buffer

let string_label : int -> string = Printf.sprintf "_s%d_"

let rec compile_args (regs : reg list) : expr list -> unit =
  function
  | [] -> ()
  | (expr :: exprs) ->
    (
      match regs with
      | [] -> assert false
      | (reg :: regs) ->
        (
          compile_expr expr;
          compile_args regs exprs;
          append_buffer (Printf.sprintf "\tpop %s\n" (show_reg reg))
        )
    )

and compile_expr : expr -> unit =
  function
  | ExprDrop expr ->
    (
      compile_expr expr;
      append_buffer "\tadd rsp, 8\n"
    )
  | ExprRet expr ->
    (
      compile_expr expr;
      append_buffer "\tret\n"
    )
  | ExprIntrin (IntrinPrintf, args) ->
    (
      compile_args arg_regs args;
      append_buffer
        "\txor eax, eax\n\
         \tcall printf\n\
         \tpush rax\n"
    )
  | ExprStr str ->
    (
      (
        match Hashtbl.find_opt context.strings str with
        | None ->
          (
            let label : string = string_label (get_k ()) in
            Hashtbl.add context.strings str label;
            label
          )
        | Some label -> label
      )
      |> Printf.sprintf "\tpush %s\n"
      |> append_buffer
    )

let compile_func (func : func) : unit =
  append_buffer (Printf.sprintf "%s:\n" func.label);
  List.iter compile_expr func.body

let () : unit =
  append_buffer
    "format ELF64\n\
     public _start\n\
     extrn printf\n\
     section '.text' executable\n\
     _start:\n\
     \tcall _entry_\n\
     \txor edi, edi\n\
     \tmov eax, 60\n\
     \tsyscall\n\
    ";
  List.iter compile_func
    [
      {
        label = "_entry_";
        args = [];
        body =
          [
            ExprRet (ExprDrop (ExprIntrin (
                IntrinPrintf,
                [
                  ExprStr "%s\n";
                  ExprStr "Hello, world!";
                ]
              )))
          ];
      };
    ];
  append_buffer "section '.rodata'\n";
  Hashtbl.iter compile_string context.strings;
  Buffer.output_buffer (open_out Sys.argv.(1)) buffer

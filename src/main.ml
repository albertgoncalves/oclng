type context =
  {
    mutable k : int;
    strings : (string, string) Hashtbl.t;
    mutable n_locals : int;
    locals : (string, int) Hashtbl.t;
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
  | ExprInt of int
  | ExprStr of string
  | ExprVar of string
  | ExprAssign of (string * expr)
  | ExprSub of (expr * expr)
  | ExprCall of (string * expr list)

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
    n_locals = 0;
    locals = Hashtbl.create 8;
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

let append_local (var : string) : unit =
  Hashtbl.add context.locals var context.n_locals;
  context.n_locals <- context.n_locals + 1

let rec compile_call_args (regs : reg list) : expr list -> unit =
  function
  | [] -> ()
  | (expr :: exprs) ->
    (
      match regs with
      | [] -> assert false
      | (reg :: regs) ->
        (
          compile_expr expr;
          compile_call_args regs exprs;
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
      append_buffer
        "\tpop rax\n\
         \tmov rsp, rbp\n\
         \tpop rbp\n\
         \tret\n";
    )
  | ExprIntrin (IntrinPrintf, args) ->
    (
      compile_call_args arg_regs args;
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
  | ExprInt x -> append_buffer (Printf.sprintf "\tpush %d\n" x)
  | ExprVar var ->
    (
      (Hashtbl.find context.locals var + 1) * 8
      |> Printf.sprintf "\tpush qword [rbp - %d]\n"
      |> append_buffer
    )
  | ExprAssign (var, expr) ->
    (
      compile_expr expr;
      append_local var
    )
  | ExprSub (l, r) ->
    (
      compile_expr l;
      compile_expr r;
      append_buffer
        "\tpop r11\n\
         \tpop r10\n\
         \tsub r10, r11\n\
         \tpush r10\n"
    )
  | ExprCall (label, args) ->
    (
      compile_call_args arg_regs args;
      append_buffer
        (Printf.sprintf
           "\tcall %s\n\
            \tpush rax\n" label)
    )

let rec compile_func_args (regs : reg list) : string list -> unit =
  function
  | [] -> ()
  | (str :: strs) ->
    (
      match regs with
      | [] -> assert false
      | (reg :: regs) ->
        (
          append_local str;
          append_buffer (Printf.sprintf "\tpush %s\n" (show_reg reg));
          compile_func_args regs strs
        )
    )
let compile_func (func : func) : unit =
  context.n_locals <- 0;
  Hashtbl.clear context.locals;
  append_buffer
    (Printf.sprintf "%s:\n\
                     \tpush rbp\n\
                     \tmov rbp, rsp\n" func.label);
  compile_func_args arg_regs func.args;
  List.iter compile_expr func.body

let () : unit =
  append_buffer
    "format ELF64\n\
     public _start\n\
     extrn printf\n\
     section '.text' executable\n\
     _start:\n\
     \tcall _entry_\n\
     \tmov rdi, rax\n\
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
            ExprAssign ("x", ExprStr "Hello, world!");
            ExprAssign
              (
                "y",
                ExprCall
                  (
                    "f",
                    [
                      ExprCall ("f", [ExprInt (-1234); ExprInt 0]);
                      ExprVar "x";
                    ]
                  )
              );
            ExprDrop
              (
                ExprIntrin
                  (
                    IntrinPrintf,
                    [
                      ExprStr "%s %ld\n";
                      ExprVar "x";
                      ExprVar "y";
                    ]
                  )
              );
            ExprRet (ExprInt 0);
          ];
      };
      {
        label = "f";
        args = ["x"; "y"];
        body = [ExprRet (ExprSub (ExprVar "x", ExprInt 1))];
      };
    ];
  append_buffer "section '.rodata'\n";
  Hashtbl.iter compile_string context.strings;
  Buffer.output_buffer (open_out Sys.argv.(1)) buffer

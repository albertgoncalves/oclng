type reg =
  | RegRdi
  | RegRsi
  | RegRdx
  | RegRcx
  | RegR8
  | RegR9
  | RegR10
  | RegR11
  | RegEax
  | RegRax
  | RegRbp
  | RegRsp

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
  | ExprIf of (expr * expr list * expr list)
  | ExprEq of (expr * expr)
  | ExprAdd of (expr * expr)
  | ExprSub of (expr * expr)
  | ExprCall of (string * expr list)

type op =
  | OpReg of reg
  | OpDeref of (reg * int)
  | OpImm of int
  | OpLabel of string

type inst =
  | InstPush of op
  | InstPop of op
  | InstDrop
  | InstMov of (op * op)
  | InstAdd of (op * op)
  | InstSub of (op * op)
  | InstXor of (op * op)
  | InstLabel of string
  | InstCall of op
  | InstJmp of op
  | InstCmp of (op * op)
  | InstJe of op
  | InstEnter
  | InstLeave
  | InstRet

type func =
  {
    label : string;
    args : string list;
    body : expr list;
  }

type context =
  {
    mutable k : int;
    strings : (string, string) Hashtbl.t;
    mutable need_stack : bool;
    mutable n_locals : int;
    locals : (string, int) Hashtbl.t;
    insts : inst Queue.t;
  }

let context : context =
  {
    k = 0;
    strings = Hashtbl.create 64;
    need_stack = false;
    n_locals = 0;
    locals = Hashtbl.create 8;
    insts = Queue.create ();
  }

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
  | RegR10 -> "r10"
  | RegR11 -> "r11"
  | RegEax -> "eax"
  | RegRax -> "rax"
  | RegRbp -> "rbp"
  | RegRsp -> "rsp"

let show_op : op -> string =
  function
  | OpReg reg -> show_reg reg
  | OpDeref (reg, 0) -> Printf.sprintf "qword [%s]" (show_reg reg)
  | OpDeref (reg, offset) when offset < 0 ->
    Printf.sprintf "qword [%s - %d]" (show_reg reg) (-offset)
  | OpDeref (reg, offset) ->
    Printf.sprintf "qword [%s + %d]" (show_reg reg) offset
  | OpImm x -> string_of_int x
  | OpLabel str -> str

let show_inst : inst -> string =
  function
  | InstPush op -> Printf.sprintf "\tpush %s\n" (show_op op)
  | InstPop op -> Printf.sprintf "\tpop %s\n" (show_op op)
  | InstDrop -> "\tadd rsp, 8\n"
  | InstMov (l, r) -> Printf.sprintf "\tmov %s, %s\n" (show_op l) (show_op r)
  | InstAdd (l, r) -> Printf.sprintf "\tadd %s, %s\n" (show_op l) (show_op r)
  | InstSub (l, r) -> Printf.sprintf "\tsub %s, %s\n" (show_op l) (show_op r)
  | InstXor (l, r) -> Printf.sprintf "\txor %s, %s\n" (show_op l) (show_op r)
  | InstLabel label -> Printf.sprintf "%s:\n" label
  | InstCall op -> Printf.sprintf "\tcall %s\n" (show_op op)
  | InstJmp op -> Printf.sprintf "\tjmp %s\n" (show_op op)
  | InstCmp (l, r) -> Printf.sprintf "\tcmp %s, %s\n" (show_op l) (show_op r)
  | InstJe op -> Printf.sprintf "\tje %s\n" (show_op op)
  | InstEnter ->
    "\tpush rbp\n\
     \tmov rbp, rsp\n"
  | InstLeave -> "\tleave\n"
  | InstRet -> "\tret\n"

let buffer : Buffer.t = Buffer.create 1024

let append_buffer : string -> unit = Buffer.add_string buffer

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

let append_inst (inst : inst) : unit =
  Queue.add inst context.insts

let append_insts : inst list -> unit = List.iter append_inst

let rec returns : expr list -> bool =
  function
  | [] -> assert false
  | [ExprRet _] -> true
  | _ :: exprs -> returns exprs

let rec compile_call_args (regs : reg list) : expr list -> unit =
  function
  | [] -> ()
  | expr :: exprs ->
    (
      match regs with
      | [] -> assert false
      | reg :: regs ->
        (
          compile_expr expr;
          compile_call_args regs exprs;
          append_inst (InstPop (OpReg reg))
        )
    )

and compile_if_condition (label_then : string) : expr -> unit =
  function
  | ExprEq (l, r) ->
    (
      compile_expr l;
      append_inst (InstPop (OpReg RegR10));
      compile_expr r;
      append_insts
        [
          InstPop (OpReg RegR11);
          InstCmp (OpReg RegR10, OpReg RegR11);
          InstJe (OpLabel label_then);
        ]
    )
  | _ -> assert false

and compile_expr : expr -> unit =
  function
  | ExprDrop expr ->
    (
      compile_expr expr;
      append_inst InstDrop
    )
  | ExprRet expr ->
    (
      compile_expr expr;
      append_inst (InstPop (OpReg RegRax));
      if context.need_stack then (
        append_inst InstLeave;
      );
      append_inst InstRet
    )
  | ExprIntrin (IntrinPrintf, args) ->
    (
      compile_call_args arg_regs args;
      append_insts
        [
          InstXor (OpReg RegEax, OpReg RegEax);
          InstCall (OpLabel "printf");
          InstPush (OpReg RegRax);
        ]
    )
  | ExprStr str ->
    (
      let label : string =
        match Hashtbl.find_opt context.strings str with
        | None ->
          (
            let label : string = string_label (get_k ()) in
            Hashtbl.add context.strings str label;
            label
          )
        | Some label -> label in
      append_inst (InstPush (OpLabel label))
    )
  | ExprInt x -> append_inst (InstPush (OpImm x))
  | ExprVar var ->
    (
      let offset : int = (Hashtbl.find context.locals var + 1) * 8 in
      append_inst (InstPush (OpDeref (RegRbp, -offset)))
    )
  | ExprAssign (var, expr) ->
    (
      compile_expr expr;
      append_local var
    )
  | ExprAdd (l, r) ->
    (
      compile_expr l;
      compile_expr r;
      append_insts
        [
          InstPop (OpReg RegR11);
          InstPop (OpReg RegR10);
          InstAdd (OpReg RegR10, OpReg RegR11);
          InstPush (OpReg RegR10);
        ]
    )
  | ExprSub (l, r) ->
    (
      compile_expr l;
      compile_expr r;
      append_insts
        [
          InstPop (OpReg RegR11);
          InstPop (OpReg RegR10);
          InstSub (OpReg RegR10, OpReg RegR11);
          InstPush (OpReg RegR10);
        ]
    )
  | ExprCall (label, args) ->
    (
      compile_call_args arg_regs args;
      append_insts
        [
          InstCall (OpLabel label);
          InstPush (OpReg RegRax);
        ]
    )
  | ExprIf (condition, exprs_then, exprs_else) ->
    (
      let then_returns : bool = returns exprs_then in
      let else_returns : bool = returns exprs_else in
      let label_then : string = Printf.sprintf "_then%d_" (get_k ()) in
      if then_returns && else_returns then (
        compile_if_condition label_then condition;
        List.iter compile_expr exprs_else;
        append_inst (InstLabel label_then);
        List.iter compile_expr exprs_then
      ) else if then_returns || else_returns then (
        assert false
      ) else (
        (* let label_else : string = Printf.sprintf "_else%d_" (get_k ()) in *)
        assert false
      )
    )
  | ExprEq _ -> assert false

let rec compile_func_args (regs : reg list) : string list -> unit =
  function
  | [] -> ()
  | str :: strs ->
    (
      match regs with
      | [] -> assert false
      | reg :: regs ->
        (
          append_local str;
          append_inst (InstPush (OpReg reg));
          compile_func_args regs strs
        )
    )

let rec any_assign : expr list -> bool =
  function
  | [] -> false
  | (ExprAssign _) :: _ -> true
  | _ :: exprs -> any_assign exprs

let compile_func (func : func) : unit =
  context.need_stack <- (List.length func.args <> 0) || any_assign func.body;
  context.n_locals <- 0;
  Hashtbl.clear context.locals;
  append_inst (InstLabel func.label);
  if context.need_stack then (
    append_inst InstEnter
  );
  compile_func_args arg_regs func.args;
  List.iter compile_expr func.body

let rec opt_push_pop : inst list -> inst list =
  function
  | [] -> []
  | InstPush op_push :: InstPop op_pop :: insts when op_push = op_pop ->
    opt_push_pop insts
  | InstPush op_push :: InstPop op_pop :: insts ->
    InstMov (op_pop, op_push) :: opt_push_pop insts
  | inst :: insts -> inst :: opt_push_pop insts

let rec opt_tail_call : inst list -> inst list =
  function
  | [] -> []
  | InstCall op :: InstLeave :: InstRet :: insts ->
    InstLeave :: InstJmp op :: opt_tail_call insts
  | InstCall op :: InstRet :: insts -> InstJmp op :: opt_tail_call insts
  | inst :: insts -> inst :: opt_tail_call insts

let () : unit =
  List.iter compile_func
    [
      {
        label = "_entry_";
        args = [];
        body =
          [
            ExprDrop
              (
                ExprIntrin
                  (
                    IntrinPrintf,
                    [
                      ExprStr "%ld\n";
                      ExprCall ("fib", [ExprInt 50; ExprInt 0; ExprInt 1]);
                    ]
                  )
              );
            ExprRet (ExprInt 0);
          ];
      };
      {
        label = "fib";
        args = ["n"; "a"; "b"];
        body =
          [
            ExprIf
              (
                (ExprEq (ExprVar "n", ExprInt 0)),
                [ExprRet (ExprVar "a")],
                [
                  ExprRet
                    (
                      ExprCall
                        (
                          "fib",
                          [
                            ExprSub (ExprVar "n", ExprInt 1);
                            ExprVar "b";
                            ExprAdd (ExprVar "a", ExprVar "b");
                          ]
                        )
                    );
                ]
              );
          ];
      };
    ];
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
  Queue.to_seq context.insts
  |> List.of_seq
  |> opt_push_pop
  |> opt_tail_call
  |> List.iter (fun inst -> append_buffer (show_inst inst));
  append_buffer "section '.rodata'\n";
  Hashtbl.iter compile_string context.strings;
  Buffer.output_buffer (open_out Sys.argv.(1)) buffer

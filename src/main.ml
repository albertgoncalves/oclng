type bin_op =
  | BinOpEq
  | BinOpAdd
  | BinOpSub

type intrin =
  | IntrinPrintf
  | IntrinPack

type call =
  | CallIntrin of intrin
  | CallLabel of string

type expr =
  | ExprDrop of expr
  | ExprRet of expr
  | ExprInt of int
  | ExprStr of string
  | ExprVar of string
  | ExprAssign of (string * expr)
  | ExprIfThen of (expr * expr list * expr list)
  | ExprBinOp of (bin_op * expr * expr)
  | ExprCall of (call * expr list)
  | ExprUnpack of (expr * ((string list) * (expr list)) list)

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

type word_size =
  | WordSizeQWord

type op =
  | OpReg of reg
  | OpDeref of (reg * word_size * int)
  | OpTable of (string * reg * int)
  | OpImm of int
  | OpLabel of string

type inst =
  | InstPush of op
  | InstPop of op
  | InstDrop of int
  | InstMov of (op * op)
  | InstAdd of (op * op)
  | InstSub of (op * op)
  | InstXor of (op * op)
  | InstLabel of string
  | InstCall of op
  | InstJmp of op
  | InstCmp of (op * op)
  | InstTest of (op * op)
  | InstJne of op
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
    mutable locals : (string, int) Hashtbl.t;
    insts : inst Queue.t;
    tables : (string * (string list)) Queue.t;
  }

let context : context =
  {
    k = 0;
    strings = Hashtbl.create 64;
    need_stack = false;
    n_locals = 0;
    locals = Hashtbl.create 8;
    insts = Queue.create ();
    tables = Queue.create ();
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

let show_word_size : word_size -> string =
  function
  | WordSizeQWord -> "qword"

let show_op : op -> string =
  function
  | OpReg reg -> show_reg reg
  | OpDeref (reg, word_size, 0) ->
    Printf.sprintf "%s [%s]" (show_word_size word_size) (show_reg reg)
  | OpDeref (reg, word_size, offset) when offset < 0 ->
    Printf.sprintf
      "%s [%s - %d]"
      (show_word_size word_size)
      (show_reg reg)
      (-offset)
  | OpDeref (reg, word_size, offset) ->
    Printf.sprintf
      "%s [%s + %d]"
      (show_word_size word_size)
      (show_reg reg)
      offset
  | OpTable (label, reg, scale) ->
    Printf.sprintf "[%s + (%s * %d)]" label (show_reg reg) scale
  | OpImm x -> string_of_int x
  | OpLabel str -> str

let show_inst : inst -> string =
  function
  | InstPush op -> Printf.sprintf "\tpush %s\n" (show_op op)
  | InstPop op -> Printf.sprintf "\tpop %s\n" (show_op op)
  | InstDrop n -> Printf.sprintf "\tadd rsp, %d\n" n
  | InstMov (l, r) -> Printf.sprintf "\tmov %s, %s\n" (show_op l) (show_op r)
  | InstAdd (l, r) -> Printf.sprintf "\tadd %s, %s\n" (show_op l) (show_op r)
  | InstSub (l, r) -> Printf.sprintf "\tsub %s, %s\n" (show_op l) (show_op r)
  | InstXor (l, r) -> Printf.sprintf "\txor %s, %s\n" (show_op l) (show_op r)
  | InstLabel label -> Printf.sprintf "%s:\n" label
  | InstCall op -> Printf.sprintf "\tcall %s\n" (show_op op)
  | InstJmp op -> Printf.sprintf "\tjmp %s\n" (show_op op)
  | InstCmp (l, r) -> Printf.sprintf "\tcmp %s, %s\n" (show_op l) (show_op r)
  | InstTest (l, r) -> Printf.sprintf "\ttest %s, %s\n" (show_op l) (show_op r)
  | InstJne op -> Printf.sprintf "\tjne %s\n" (show_op op)
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

let compile_table ((table, branches) : (string * string list)) : unit =
  append_buffer
    (Printf.sprintf "\t%s dq %s\n" table (String.concat "," branches))

let string_label : int -> string = Printf.sprintf "_s%d_"

let append_local (var : string) : unit =
  Hashtbl.add context.locals var context.n_locals;
  context.n_locals <- context.n_locals + 1

let append_inst (inst : inst) : unit =
  Queue.add inst context.insts

let append_insts : inst list -> unit = List.iter append_inst

let is_assign : expr -> bool =
  function
  | ExprAssign _-> true
  | _ -> false

let rec returns : expr list -> bool =
  function
  | [] -> assert false
  | [ExprIfThen (_, exprs_then, exprs_else)] ->
    if returns exprs_then then (
      assert (returns exprs_else);
      true
    ) else (
      assert (not (returns exprs_else));
      false
    )
  | [ExprRet _] -> true
  | _ :: exprs -> returns exprs

let rec compile_pack_args (offset : int) : string list -> unit =
  function
  | [] -> ()
  | str :: strs ->
    (
      append_local str;
      append_inst (InstPush (OpDeref (RegR11, WordSizeQWord, 8 * offset)));
      compile_pack_args (offset + 1) strs
    )

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

and compile_if_condition (label_else : string) : expr -> unit =
  function
  | ExprBinOp (BinOpEq, expr, ExprInt 0)
  | ExprBinOp (BinOpEq, ExprInt 0, expr) ->
    (
      compile_expr expr;
      append_insts
        [
          InstPop (OpReg RegR10);
          InstTest (OpReg RegR10, OpReg RegR10);
          InstJne (OpLabel label_else);
        ]
    )
  | ExprBinOp (BinOpEq, l, r) ->
    (
      compile_expr l;
      compile_expr r;
      append_insts
        [
          InstPop (OpReg RegR11);
          InstPop (OpReg RegR10);
          InstCmp (OpReg RegR10, OpReg RegR11);
          InstJne (OpLabel label_else);
        ]
    )
  | _ -> assert false

and compile_expr : expr -> unit =
  function
  | ExprDrop expr ->
    (
      compile_expr expr;
      append_inst (InstDrop 8)
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
  | ExprCall (call, args) ->
    (
      compile_call_args arg_regs args;
      (match call with
       | CallIntrin IntrinPrintf ->
         append_insts
           [
             InstXor (OpReg RegEax, OpReg RegEax);
             InstCall (OpLabel "printf");
           ]
       | CallIntrin IntrinPack ->
         (match List.length args with
          | 1 -> append_inst (InstCall (OpLabel "pack_1"))
          | 2 -> append_inst (InstCall (OpLabel "pack_2"))
          | 3 -> append_inst (InstCall (OpLabel "pack_3"))
          | _ -> assert false);
       | CallLabel label -> append_inst (InstCall (OpLabel label)));
      append_inst (InstPush (OpReg RegRax))
    )
  | ExprStr str ->
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
  | ExprInt x -> append_inst (InstPush (OpImm x))
  | ExprVar var ->
    let offset : int = -((Hashtbl.find context.locals var + 1) * 8) in
    append_inst (InstPush (OpDeref (RegRbp, WordSizeQWord, offset)))
  | ExprAssign (var, expr) ->
    (
      compile_expr expr;
      append_local var
    )
  | ExprBinOp (bin_op, l, r) ->
    (
      compile_expr l;
      compile_expr r;
      append_insts
        [
          InstPop (OpReg RegR11);
          InstPop (OpReg RegR10);
          (match bin_op with
           | BinOpAdd -> InstAdd (OpReg RegR10, OpReg RegR11)
           | BinOpSub -> InstSub (OpReg RegR10, OpReg RegR11)
           | _ -> assert false);
          InstPush (OpReg RegR10);
        ]
    )
  | ExprIfThen (condition, exprs_then, exprs_else) ->
    (
      assert (not (is_assign condition));
      let returns_then : bool = returns exprs_then in
      let returns_else : bool = returns exprs_else in
      let label_else : string = Printf.sprintf "_else%d_" (get_k ()) in
      let n_locals : int = context.n_locals in
      let locals : (string, int) Hashtbl.t = Hashtbl.copy context.locals in
      if returns_then && returns_else then (
        compile_if_condition label_else condition;
        List.iter compile_expr exprs_then;
        if context.n_locals <> n_locals then (
          assert (n_locals < context.n_locals);
          context.n_locals <- n_locals;
          context.locals <- locals
        );
        append_inst (InstLabel label_else);
        List.iter compile_expr exprs_else;
        if context.n_locals <> n_locals then (
          assert (n_locals < context.n_locals);
          context.n_locals <- n_locals;
          context.locals <- locals
        )
      ) else (
        assert false
      )
    )
  | ExprUnpack (packed_expr, branches) ->
    (
      assert (not (is_assign packed_expr));
      let label_table : string = Printf.sprintf "_table%d_" (get_k ()) in
      let label_end : string = Printf.sprintf "_end%d_" (get_k ()) in
      compile_expr packed_expr;
      append_insts
        [
          InstPop (OpReg RegR11);
          InstMov (OpReg RegR10, OpDeref (RegR11, WordSizeQWord, 0));
          InstJmp (OpTable (label_table, RegR10, 8));
        ];
      let n_locals : int = context.n_locals in
      let locals : (string, int) Hashtbl.t = Hashtbl.copy context.locals in
      let label_branches : string list =
        List.map (compile_branch n_locals locals label_end) branches in
      append_insts
        [
          InstLabel label_end;
          InstPush (OpReg RegRax);
        ];
      Queue.add (label_table, label_branches) context.tables
    )

and compile_branch
    (n_locals : int)
    (locals : (string, int) Hashtbl.t)
    (label_end : string)
    ((args, exprs) : (string list * expr list)) : string =
  let label_branch : string = Printf.sprintf "_branch%d_" (get_k ()) in
  append_inst (InstLabel label_branch);
  compile_pack_args 1 args;
  List.iter compile_expr exprs;
  append_inst (InstPop (OpReg RegRax));
  let new_n_locals : int = context.n_locals in
  if new_n_locals <> n_locals then (
    assert (n_locals < new_n_locals);
    append_inst (InstDrop (8 * (new_n_locals - n_locals)))
  );
  append_inst (InstJmp (OpLabel label_end));
  context.n_locals <- n_locals;
  context.locals <- locals;
  label_branch

let rec need_stack : expr list -> bool =
  function
  | [] -> false
  | (ExprAssign _) :: _ -> true
  | (ExprUnpack _) :: _ -> true
  | (ExprIfThen (ExprAssign _, _, _)) :: _ -> assert false
  | (ExprIfThen (_, exprs_then, exprs_else)) :: exprs ->
    (need_stack exprs_then) || (need_stack exprs_else) || (need_stack exprs)
  | _ :: exprs -> need_stack exprs

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

let rec prepare : expr list -> expr list =
  function
  | ExprRet _ :: _
  | ExprDrop _ :: _
  | [ExprAssign _] -> assert false
  | [] -> []
  | [ExprIfThen (condition, exprs_then, exprs_else)] ->
    [ExprIfThen (condition, prepare exprs_then, prepare exprs_else)]
  | [expr] -> [ExprRet expr]
  | (ExprAssign _ as expr) :: exprs -> expr :: prepare exprs
  | expr :: exprs -> ExprDrop expr :: prepare exprs

let compile_func (func : func) : unit =
  context.need_stack <- (List.length func.args <> 0) || (need_stack func.body);
  context.n_locals <- 0;
  Hashtbl.clear context.locals;
  append_inst (InstLabel func.label);
  if context.need_stack then (
    append_inst InstEnter
  );
  compile_func_args arg_regs func.args;
  assert ((List.length func.body) <> 0);
  List.iter compile_expr (prepare func.body)

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
            ExprCall
              (
                CallIntrin IntrinPrintf,
                [
                  ExprStr "%ld\n";
                  ExprCall
                    (CallLabel "fib", [ExprInt 50; ExprInt 0; ExprInt 1]);
                ]
              );
            ExprUnpack
              (
                ExprCall
                  (
                    CallIntrin IntrinPack,
                    [ExprInt 1; ExprStr "%s\n"; ExprStr "Here!"]
                  ),
                [
                  (
                    [],
                    [ExprCall (CallIntrin IntrinPrintf, [ExprStr "!\n"])]
                  );
                  (
                    ["a"; "b"],
                    [
                      ExprCall
                        (
                          CallIntrin IntrinPrintf,
                          [ExprVar "a"; ExprVar "b"]
                        )
                    ]
                  );
                ]
              );
            ExprInt 0;
          ];
      };
      {
        label = "fib";
        args = ["n"; "a"; "b"];
        body =
          [
            ExprIfThen
              (
                (ExprBinOp (BinOpEq, ExprVar "n", ExprInt 0)),
                [ExprVar "a"],
                [
                  ExprCall
                    (
                      CallLabel "fib",
                      [
                        ExprBinOp (BinOpSub, ExprVar "n", ExprInt 1);
                        ExprVar "b";
                        ExprBinOp (BinOpAdd, ExprVar "a", ExprVar "b");
                      ]
                    )
                ]
              );
          ];
      };
    ];
  append_buffer
    "format ELF64\n\
     public _entry_\n\
     extrn printf\n\
     extrn pack_1\n\
     extrn pack_2\n\
     extrn pack_3\n";
  Queue.to_seq context.insts
  |> List.of_seq
  |> opt_push_pop
  |> opt_tail_call
  |> List.iter (fun inst -> append_buffer (show_inst inst));
  append_buffer "section '.rodata'\n";
  Hashtbl.iter compile_string context.strings;
  Queue.iter compile_table context.tables;
  Buffer.output_buffer (open_out Sys.argv.(1)) buffer

open Types

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
  | InstInc of op
  | InstDec of op
  | InstLabel of string
  | InstCall of op
  | InstJmp of op
  | InstCmp of (op * op)
  | InstTest of (op * op)
  | InstJne of op
  | InstEnter
  | InstLeave
  | InstRet

type context =
  {
    mutable k : int;
    strings : (string, string) Hashtbl.t;
    mutable need_stack : bool;
    mutable n_locals : int;
    mutable locals : (string, int) Hashtbl.t;
    insts : inst Queue.t;
    tables : (string * (string list)) Queue.t;
    externs : (string, unit) Hashtbl.t;
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
    externs = Hashtbl.create 8;
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
  | InstInc op -> Printf.sprintf "\tinc %s\n" (show_op op)
  | InstDec op -> Printf.sprintf "\tdec %s\n" (show_op op)
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

let compile_string (str : string) (label : string) : string =
  String.to_seq str
  |> Seq.map Char.code
  |> Seq.map string_of_int
  |> List.of_seq
  |> String.concat ","
  |> Printf.sprintf "\t%s db %s,0\n" label

let compile_table ((table, branches) : (string * string list)) : string =
  Printf.sprintf "\t%s dq %s\n" table (String.concat "," branches)

let string_label : int -> string = Printf.sprintf "_s%d_"

let append_local (var : string) : unit =
  assert (not (Hashtbl.mem context.locals var));
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
  | [ExprUnpack (_, [(_, exprs)])] -> returns exprs
  | [ExprUnpack (_, branches)] -> returns_branches branches
  | [ExprRet _] -> true
  | [_] -> false
  | _ :: exprs -> returns exprs

and returns_branches : branch list -> bool =
  function
  | (_, exprs) :: branches ->
    if returns exprs then (
      assert (List.for_all (fun x -> returns (snd x)) branches);
      true
    ) else (
      assert (List.for_all (fun x -> not (returns (snd x))) branches);
      false
    )
  | [] -> assert false

let rec compile_pack_args (offset : int) : string list -> unit =
  function
  | [] -> ()
  | str :: strs ->
    (
      append_inst (InstPush (OpDeref (RegR11, WordSizeQWord, 8 * offset)));
      append_local str;
      compile_pack_args (offset + 1) strs
    )

let get_local (n : int) : op =
  let offset : int = -((n + 1) * 8) in
  OpDeref (RegRbp, WordSizeQWord, offset)

let rec compile_expr : expr -> unit =
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
         (
           Hashtbl.replace context.externs "printf" ();
           append_insts
             [
               InstXor (OpReg RegEax, OpReg RegEax);
               InstCall (OpLabel "printf");
             ]
         )
       | CallIntrin IntrinPack ->
         (
           let label : string =
             match List.length args with
             | 1 -> "pack_1"
             | 2 -> "pack_2"
             | 3 -> "pack_3"
             | 4 -> "pack_4"
             | 5 -> "pack_5"
             | 6 -> "pack_6"
             | _ -> assert false in
           Hashtbl.replace context.externs label ();
           append_inst (InstCall (OpLabel label))
         )
       | CallLabel label ->
         match Hashtbl.find_opt context.locals label with
         | None -> append_inst (InstCall (OpLabel label))
         | Some n -> append_inst (InstCall (get_local n)));
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
    (match Hashtbl.find_opt context.locals var with
     | Some n -> append_inst (InstPush (get_local n))
     | None -> append_inst (InstPush (OpLabel var)))
  | ExprAssign (var, expr) ->
    (
      compile_expr expr;
      append_local var
    )
  | ExprInject (pointer, n, replacement) ->
    (
      compile_expr pointer;
      compile_expr replacement;
      append_insts
        [
          InstPop (OpReg RegR11);
          InstPop (OpReg RegR10);
          InstMov ((OpDeref (RegR10, WordSizeQWord, n * 8)), (OpReg RegR11));
          InstPush (OpReg RegR10);
        ]
    )
  | ExprBinOp (BinOpAdd, expr, ExprInt 1)
  | ExprBinOp (BinOpAdd, ExprInt 1, expr) ->
    (
      compile_expr expr;
      append_insts
        [
          InstPop (OpReg RegR10);
          InstInc (OpReg RegR10);
          InstPush (OpReg RegR10);
        ]
    )
  | ExprBinOp (BinOpSub, expr, ExprInt 1) ->
    (
      compile_expr expr;
      append_insts
        [
          InstPop (OpReg RegR10);
          InstDec (OpReg RegR10);
          InstPush (OpReg RegR10);
        ]
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
  | ExprIf (condition, exprs_then) ->
    (
      let label_else : string = Printf.sprintf "_else%d_" (get_k ()) in
      let n_locals : int = context.n_locals in
      let locals : (string, int) Hashtbl.t = Hashtbl.copy context.locals in
      compile_if_condition label_else condition;
      List.iter compile_expr exprs_then;
      if context.n_locals <> n_locals then (
        assert (n_locals < context.n_locals);
        context.n_locals <- n_locals;
        context.locals <- locals
      );
      append_inst (InstLabel label_else);
    )
  | ExprIfThen (condition, exprs_then, exprs_else) ->
    (
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
  | ExprUnpack (packed, branches) ->
    (
      let label_table : string = Printf.sprintf "_table%d_" (get_k ()) in
      let label_end : string = Printf.sprintf "_end%d_" (get_k ()) in
      compile_expr packed;
      append_insts
        [
          InstPop (OpReg RegR11);
          InstMov (OpReg RegR10, OpDeref (RegR11, WordSizeQWord, 0));
          InstJmp (OpTable (label_table, RegR10, 8));
        ];
      let label_branches : string list =
        List.map (compile_branch label_end) branches in
      if not (returns_branches branches) then (
        append_insts
          [
            InstLabel label_end;
            InstPush (OpReg RegRax);
          ]
      );
      Queue.add (label_table, label_branches) context.tables
    )

and compile_call_args (regs : reg list) : expr list -> unit =
  function
  | [] -> ()
  | expr :: exprs ->
    (
      assert (not (is_assign expr));
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

and compile_branch (label_end : string) ((args, exprs) : branch) : string =
  let label_branch : string = Printf.sprintf "_branch%d_" (get_k ()) in
  append_inst (InstLabel label_branch);
  let n_locals : int = context.n_locals in
  let locals : (string, int) Hashtbl.t = Hashtbl.copy context.locals in
  compile_pack_args 1 args;
  let returns_exprs : bool = returns exprs in
  List.iter compile_expr exprs;
  if not returns_exprs then (
    append_inst (InstPop (OpReg RegRax))
  );
  let new_n_locals : int = context.n_locals in
  if new_n_locals <> n_locals then (
    assert (n_locals < new_n_locals);
    if not returns_exprs then (
      append_inst (InstDrop (8 * (new_n_locals - n_locals)))
    );
    context.n_locals <- n_locals;
    context.locals <- locals
  );
  if not returns_exprs then (
    append_inst (InstJmp (OpLabel label_end))
  );
  label_branch

let rec need_stack : expr list -> bool =
  function
  | [] -> false
  | (ExprAssign _) :: _
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
          append_inst (InstPush (OpReg reg));
          append_local str;
          compile_func_args regs strs
        )
    )

let rec return_last : expr list -> expr list =
  function
  | ExprRet _ :: _
  | ExprDrop _ :: _
  | [ExprIf _]
  | [ExprAssign _] -> assert false
  | [] -> []
  | [ExprIfThen (condition, exprs_then, exprs_else)] ->
    [ExprIfThen (condition, return_last exprs_then, return_last exprs_else)]
  | [ExprUnpack (packed, branches)] ->
    [
      ExprUnpack
        (
          packed,
          List.map (fun (args, exprs) -> (args, return_last exprs)) branches
        )
    ]
  | [expr] -> [ExprRet expr]
  | (ExprIf _ as expr) :: exprs
  | (ExprAssign _ as expr) :: exprs -> expr :: return_last exprs
  | expr :: exprs -> ExprDrop expr :: return_last exprs

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
  List.iter compile_expr (return_last func.body)

let rec opt_push_pop : inst list -> inst list =
  function
  | [] -> []
  | InstPush op_push :: InstPop op_pop :: insts when op_push = op_pop ->
    opt_push_pop insts
  | InstPush op_push :: InstPop op_pop :: insts ->
    InstMov (op_pop, op_push) :: opt_push_pop insts
  | InstPush _ :: InstDrop _ :: insts -> opt_push_pop insts
  | inst :: insts -> inst :: opt_push_pop insts

let rec opt_tail_call : inst list -> inst list =
  function
  | [] -> []
  | InstCall op :: InstLeave :: InstRet :: insts ->
    InstLeave :: InstJmp op :: opt_tail_call insts
  | InstCall op :: InstRet :: insts -> InstJmp op :: opt_tail_call insts
  | inst :: insts -> inst :: opt_tail_call insts

let rec opt_jump : inst list -> inst list =
  function
  | [] -> []
  | InstJmp (OpLabel label0) :: InstLabel label1 :: insts
    when label0 = label1 -> InstLabel label1 :: opt_jump insts
  | inst :: insts -> inst :: opt_jump insts

let compile (funcs : func list) : Buffer.t =
  List.iter compile_func funcs;
  let buffer : Buffer.t = Buffer.create 1024 in
  Buffer.add_string buffer
    "format ELF64\n\
     public _entry_\n";
  Hashtbl.iter
    (fun k _ -> Buffer.add_string buffer (Printf.sprintf "extrn %s\n" k))
    context.externs;
  Queue.to_seq context.insts
  |> List.of_seq
  |> opt_push_pop
  |> opt_tail_call
  |> opt_jump
  |> List.iter (fun inst -> Buffer.add_string buffer (show_inst inst));
  Buffer.add_string buffer "section '.rodata'\n";
  Hashtbl.iter
    (fun k v -> Buffer.add_string buffer (compile_string k v))
    context.strings;
  Queue.iter
    (fun t -> Buffer.add_string buffer (compile_table t))
    context.tables;
  buffer

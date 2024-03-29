type reg =
  | RegRdi
  | RegEdi
  | RegDi
  | RegRsi
  | RegSil
  | RegRdx
  | RegRcx
  | RegR8
  | RegR9
  | RegR10
  | RegR10b
  | RegR11
  | RegRax
  | RegEax
  | RegRbp
  | RegRsp

type op =
  | OpReg of reg
  | OpDerefRegOffset of (reg * int)
  | OpDerefLabelReg of (string * reg)
  | OpImm of int
  | OpLabel of string

type inst =
  | InstPush of op
  | InstPop of op
  | InstDrop of int
  | InstMov of (op * op)
  | InstAdd of (op * op)
  | InstSub of (op * op)
  | InstIMul of (op * op)
  | InstIDiv of op
  | InstCmovns of (op * op)
  | InstCmovz of (op * op)
  | InstCqo
  | InstAnd of (op * op)
  | InstXor of (op * op)
  | InstLabel of string
  | InstCall of op
  | InstJmp of op
  | InstCmp of (op * op)
  | InstTest of (op * op)
  | InstSete of op
  | InstSets of op
  | InstRet

type context =
  {
    mutable k : int;
    mutable stack : int;
    mutable base : int;
    vars : (string, int) Hashtbl.t;
    insts : inst Queue.t;
    strings : (string, string) Hashtbl.t;
    tables : (string * (string list)) Queue.t;
    externs : (string, unit) Hashtbl.t;
  }

let context : context =
  {
    k = 0;
    stack = 0;
    base = 0;
    vars = Hashtbl.create 8;
    insts = Queue.create ();
    strings = Hashtbl.create 64;
    tables = Queue.create ();
    externs = Hashtbl.create 8;
  }

let arg_regs : reg list =
  [RegRdi; RegRsi; RegRdx; RegRcx; RegR8; RegR9]

let get_k () : int =
  let k : int = context.k in
  context.k <- context.k + 1;
  k

let show_reg : reg -> string =
  function
  | RegRdi -> "rdi"
  | RegEdi -> "edi"
  | RegDi -> "di"
  | RegRsi -> "rsi"
  | RegSil -> "sil"
  | RegRdx -> "rdx"
  | RegRcx -> "rcx"
  | RegR8 -> "r8"
  | RegR9 -> "r9"
  | RegR10 -> "r10"
  | RegR10b -> "r10b"
  | RegR11 -> "r11"
  | RegRax -> "rax"
  | RegEax -> "eax"
  | RegRbp -> "rbp"
  | RegRsp -> "rsp"

let show_op : op -> string =
  function
  | OpReg reg -> show_reg reg
  | OpDerefRegOffset (reg, 0) -> Printf.sprintf "qword [%s]" (show_reg reg)
  | OpDerefRegOffset (reg, offset) when offset < 0 ->
    Printf.sprintf "qword [%s - %d]" (show_reg reg) (offset * -8)
  | OpDerefRegOffset (reg, offset) ->
    Printf.sprintf "qword [%s + %d]" (show_reg reg) (offset * 8)
  | OpDerefLabelReg (label, reg) ->
    Printf.sprintf "[%s + (%s * 8)]" label (show_reg reg)
  | OpImm x -> string_of_int x
  | OpLabel str -> str

let show_inst : inst -> string =
  function
  | InstPush op -> Printf.sprintf "\tpush %s\n" (show_op op)
  | InstPop op -> Printf.sprintf "\tpop %s\n" (show_op op)
  | InstDrop 0 -> ""
  | InstDrop n -> Printf.sprintf "\tadd rsp, %d\n" (n * 8)
  | InstMov (l, r) -> Printf.sprintf "\tmov %s, %s\n" (show_op l) (show_op r)
  | InstAdd (l, r) -> Printf.sprintf "\tadd %s, %s\n" (show_op l) (show_op r)
  | InstSub (l, r) -> Printf.sprintf "\tsub %s, %s\n" (show_op l) (show_op r)
  | InstIMul (l, r) -> Printf.sprintf "\timul %s, %s\n" (show_op l) (show_op r)
  | InstIDiv op -> Printf.sprintf "\tidiv %s\n" (show_op op)
  | InstCmovns (l, r) ->
    Printf.sprintf "\tcmovns %s, %s\n" (show_op l) (show_op r)
  | InstCmovz (l, r) ->
    Printf.sprintf "\tcmovz %s, %s\n" (show_op l) (show_op r)
  | InstCqo -> "\tcqo\n"
  | InstAnd (l, r) -> Printf.sprintf "\tand %s, %s\n" (show_op l) (show_op r)
  | InstXor (l, r) -> Printf.sprintf "\txor %s, %s\n" (show_op l) (show_op r)
  | InstLabel label -> Printf.sprintf "%s:\n" label
  | InstCall op -> Printf.sprintf "\tcall %s\n" (show_op op)
  | InstJmp op -> Printf.sprintf "\tjmp %s\n" (show_op op)
  | InstCmp (l, r) -> Printf.sprintf "\tcmp %s, %s\n" (show_op l) (show_op r)
  | InstTest (l, r) -> Printf.sprintf "\ttest %s, %s\n" (show_op l) (show_op r)
  | InstSete op -> Printf.sprintf "\tsete %s\n" (show_op op)
  | InstSets op -> Printf.sprintf "\tsets %s\n" (show_op op)
  | InstRet -> "\tret\n"

let string_label : int -> string =
  Printf.sprintf "_str_%d_"

let append_inst (inst : inst) : unit =
  Queue.add inst context.insts

let append_insts : inst list -> unit =
  List.iter append_inst

let get_var (n : int) : op =
  OpDerefRegOffset (RegRsp, context.stack - n)

let compile_string (label : string) : string -> string =
  function
  | "" -> Printf.sprintf "\t%s db 0\n" label
  | s -> Printf.sprintf "\t%s db %s,0\n" label (Parse.show_string s)

let compile_table ((table, branches) : (string * string list)) : string =
  Printf.sprintf "\t%s dq %s\n" table (String.concat "," branches)

let append_var (var : string) : unit =
  assert (not (Hashtbl.mem context.vars var));
  Hashtbl.add context.vars var context.stack

let rec compile_func_args
    (regs : reg list) : Parse.string_type_pos list -> unit =
  function
  | [] -> ()
  | (arg, _, _) :: args ->
    (match regs with
     | [] -> assert false
     | reg :: regs ->
       (
         append_inst (InstPush (OpReg reg));
         context.stack <- context.stack + 1;
         append_var arg;
         compile_func_args regs args
       ))

let rec compile_expr : Parse.expr_pos -> unit =
  function
  | (ExprInt x, _)
  | (ExprIndex (x, _, _), _)->
    (
      append_inst (InstPush (OpImm x));
      context.stack <- context.stack + 1
    )
  | (ExprStr str, _) ->
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
      append_inst (InstPush (OpLabel label));
      context.stack <- context.stack + 1
    )
  | (ExprFunc func, _) ->
    (
      append_inst (InstPush (OpLabel (fst func.label)));
      context.stack <- context.stack + 1
    )
  | (ExprVar var, _) ->
    (
      append_inst
        (match Hashtbl.find_opt context.vars var with
         | Some n -> InstPush (get_var n)
         | None -> InstPush (OpLabel var));
      context.stack <- context.stack + 1
    )
  | (ExprCall ((ExprVar label, _), args), _) ->
    if not (compile_intrinsic label args) then (
      compile_call_label label args
    )
  | (ExprCall (expr, args), _) ->
    (
      compile_call_args arg_regs args;
      compile_expr expr;
      append_inst (InstPop (OpReg RegR10));
      context.stack <- context.stack - 1;
      append_insts
        [
          InstCall (OpReg RegR10);
          InstPush (OpReg RegRax);
        ];
      context.stack <- context.stack + 1
    )
  | (ExprSwitch (expr, branches), _) -> compile_switch expr branches

and compile_call_args (regs : reg list) : Parse.expr_pos list -> unit =
  function
  | [] -> ()
  | expr :: exprs ->
    (match regs with
     | [] -> assert false
     | reg :: regs ->
       (
         compile_expr expr;
         compile_call_args regs exprs;
         append_inst (InstPop (OpReg reg));
         context.stack <- context.stack - 1;
       ))

and compile_intrinsic (label : string) (args : Parse.expr_pos list) : bool =
  match label with
  | "=" | "+" | "-" | "*" ->
    (match args with
     | [l; r] ->
       (
         compile_expr l;
         compile_expr r;
         append_insts
           [
             InstPop (OpReg RegR11);
             InstPop (OpReg RegR10);
           ];
         append_insts
           (match label with
            | "=" ->
              [
                InstCmp (OpReg RegR10, OpReg RegR11);
                InstSete (OpReg RegR10b);
                InstAnd (OpReg RegR10, OpImm 1);
              ]
            | "+" -> [InstAdd (OpReg RegR10, OpReg RegR11)]
            | "-" -> [InstSub (OpReg RegR10, OpReg RegR11)]
            | "*" -> [InstIMul (OpReg RegR10, OpReg RegR11)]
            | _ -> assert false);
         append_inst (InstPush (OpReg RegR10));
         context.stack <- context.stack - 1;
         true
       )
     | _ -> assert false)
  | "/" ->
    (match args with
     | [l; r] ->
       (
         compile_expr l;
         compile_expr r;
         append_insts
           [
             InstXor (OpReg RegR10, OpReg RegR10);

             InstPop (OpReg RegR11);
             InstPop (OpReg RegRax);
             InstCqo;
             InstIDiv (OpReg RegR11);

             InstTest (OpReg RegRax, OpReg RegRax);
             InstSets (OpReg RegR10b);
             InstTest (OpReg RegRdx, OpReg RegRdx);
             InstCmovz (OpReg RegR10, OpReg RegRdx);
             InstSub (OpReg RegRax, OpReg RegR10);

             InstPush (OpReg RegRax);
           ];
         context.stack <- context.stack - 1;
         true
       )
     | _ -> assert false)
  | "%" ->
    (match args with
     | [l; r] ->
       (
         compile_expr l;
         compile_expr r;
         append_insts
           [
             InstXor (OpReg RegRcx, OpReg RegRcx);

             InstPop (OpReg RegR11);
             InstPop (OpReg RegRax);
             InstMov (OpReg RegR10, OpReg RegR11);
             InstCqo;
             InstIDiv (OpReg RegR11);

             InstTest (OpReg RegRax, OpReg RegRax);
             InstCmovns (OpReg RegR10, OpReg RegRcx);
             InstTest (OpReg RegRdx, OpReg RegRdx);
             InstCmovz (OpReg RegR10, OpReg RegRcx);
             InstAdd (OpReg RegRdx, OpReg RegR10);

             InstPush (OpReg RegRdx);
           ];
         context.stack <- context.stack - 1;
         true
       )
     | _ -> assert false)
  | "alloc" ->
    (match args with
     | [(ExprInt n, _)] ->
       (
         assert (n < 32);
         append_insts
           [
             InstMov (OpReg RegDi, OpImm (n * 8));
             InstCall (OpLabel "alloc");
             InstPush (OpReg RegRax);
           ];
         context.stack <- context.stack + 1;
         Hashtbl.replace context.externs "alloc" ();
         true
       )
     | _ -> assert false)
  | "mask" ->
    (match args with
     | [expr] ->
       (
         compile_expr expr;
         append_insts
           [
             InstPop (OpReg RegRdi);
             InstCall (OpLabel "mask");
             InstPush (OpReg RegRax);
           ];
         Hashtbl.replace context.externs "mask" ();
         true
       )
     | _ -> assert false)
  | "unmask" ->
    (match args with
     | [expr] ->
       (
         compile_expr expr;
         append_insts
           [
             InstPop (OpReg RegRdi);
             InstCall (OpLabel "unmask");
             InstPush (OpReg RegRax);
           ];
         Hashtbl.replace context.externs "unmask" ();
         true
       )
     | _ -> assert false)
  | "free" ->
    (match args with
     | [] ->
       (
         append_insts
           [
             InstMov (OpReg RegRdi, OpReg RegRbp);
             InstMov (OpReg RegRsi, OpReg RegRsp);
             InstCall (OpLabel "free");
             InstPush (OpReg RegRax);
           ];
         context.stack <- context.stack + 1;
         Hashtbl.replace context.externs "free" ();
         true
       )
     | _ -> assert false)
  | "deref" ->
    (match args with
     | [expr; (ExprInt offset, _)] ->
       (
         compile_expr expr;
         append_insts
           [
             InstPop (OpReg RegR11);
             InstPush (OpDerefRegOffset (RegR11, offset));
           ];
         true
       )
     | _ -> assert false)
  | "child+" ->
    (match args with
     | [expr; (ExprInt n, _)] ->
       (
         assert (n < 32);
         compile_expr expr;
         append_insts
           [
             InstMov (OpReg RegRdi, OpDerefRegOffset (RegRsp, 0));
             InstMov (OpReg RegSil, OpImm n);
             InstCall (OpLabel "set_child");
             InstPush (OpImm 0);
           ];
         context.stack <- context.stack + 1;
         Hashtbl.replace context.externs "set_child" ();
         true
       )
     | _ -> assert false)
  | "child-" ->
    (match args with
     | [expr; (ExprInt n, _)] ->
       (
         assert (n < 32);
         compile_expr expr;
         append_insts
           [
             InstMov (OpReg RegRdi, OpDerefRegOffset (RegRsp, 0));
             InstMov (OpReg RegSil, OpImm n);
             InstCall (OpLabel "unset_child");
             InstPush (OpImm 0);
           ];
         context.stack <- context.stack + 1;
         Hashtbl.replace context.externs "unset_child" ();
         true
       )
     | _ -> assert false)
  | "print_stack" ->
    (match args with
     | [] ->
       (
         append_insts
           [
             InstMov (OpReg RegRdi, OpReg RegRbp);
             InstMov (OpReg RegRsi, OpReg RegRsp);
             InstCall (OpLabel "print_stack");
             InstPush (OpImm 0);
           ];
         context.stack <- context.stack + 1;
         Hashtbl.replace context.externs "print_stack" ();
         true
       )
     | _ -> assert false)
  | "printf" ->
    (
      compile_call_args arg_regs args;
      append_insts
        [
          InstXor (OpReg RegEax, OpReg RegEax);
          InstCall (OpLabel "printf");
          InstPush (OpReg RegEax);
        ];
      context.stack <- context.stack + 1;
      Hashtbl.replace context.externs "printf" ();
      true
    )
  | _ -> false

and compile_call_label (label : string) (args : Parse.expr_pos list) : unit =
  compile_call_args arg_regs args;
  append_insts
    [
      InstCall
        (match Hashtbl.find_opt context.vars label with
         | None -> OpLabel label
         | Some n -> get_var n);
      InstPush (OpReg RegRax);
    ];
  context.stack <- context.stack + 1

and compile_branch
    (base : int)
    (vars : string list)
    (label_end : string)
    (stmts : Parse.stmt_pos list) : (string * bool) =
  let label_branch : string = Printf.sprintf "_branch_%d_" (get_k ()) in
  append_inst (InstLabel label_branch);
  context.base <- context.stack;
  let returned : bool = compile_stmts stmts in
  if not returned then (
    append_inst (InstPop (OpReg RegRax));
    context.stack <- context.stack - 1;
    let garbage : int = context.stack - context.base in
    assert (0 <= garbage);
    if 0 < garbage then (
      append_inst (InstDrop garbage);
    );
    append_inst (InstJmp (OpLabel label_end))
  );
  context.stack <- context.base;
  context.base <- base;
  Seq.iter
    (fun k -> Hashtbl.remove context.vars k)
    (
      Seq.filter
        (fun k -> not (List.mem k vars))
        (Hashtbl.to_seq_keys context.vars)
    );
  (label_branch, returned)

and compile_switch
    (expr : Parse.expr_pos)
    (branches : Parse.stmt_pos list list) : unit =
  let label_table : string = Printf.sprintf "_table_%d_" (get_k ()) in
  let label_end : string = Printf.sprintf "_end_%d_" (get_k ()) in
  compile_expr expr;
  append_insts
    [
      InstPop (OpReg RegR10);
      InstJmp (OpDerefLabelReg (label_table, RegR10));
    ];
  context.stack <- context.stack - 1;
  let base : int = context.base in
  let vars : string list = List.of_seq (Hashtbl.to_seq_keys context.vars) in
  let (label_branches, returns) : (string list * bool list) =
    List.split (List.map (compile_branch base vars label_end) branches) in
  if not (List.for_all (fun x -> x) returns) then (
    append_insts
      [
        InstLabel label_end;
        InstPush (OpReg RegRax);
      ];
    context.stack <- context.stack + 1;
  );
  Queue.add (label_table, label_branches) context.tables

and compile_return () : unit =
  append_inst (InstPop (OpReg RegRax));
  context.stack <- context.stack - 1;
  if context.stack <> 0 then (
    append_inst (InstDrop context.stack)
  );
  append_inst InstRet

and compile_stmt : Parse.stmt_pos -> unit =
  function
  | (StmtHold expr, _) -> compile_expr expr
  | (StmtDrop expr, _) ->
    (
      compile_expr expr;
      append_inst (InstDrop 1);
      context.stack <- context.stack - 1
    )
  | (StmtLet (_, (ExprFunc _, _)), _) -> assert false
  | (StmtLet (var, expr), _) ->
    (
      compile_expr expr;
      append_var var
    )
  | (StmtSetLocal (var, expr), _) ->
    (
      compile_expr expr;
      context.stack <- context.stack - 1;
      append_inst
        (match Hashtbl.find_opt context.vars var with
         | Some n -> InstPop (get_var n)
         | None -> assert false)
    )
  | (StmtSetHeap (var, offset, value), _) ->
    (
      compile_expr var;
      compile_expr value;
      append_insts
        [
          InstPop (OpReg RegR11);
          InstPop (OpReg RegR10);
          InstMov (OpDerefRegOffset (RegR10, offset), OpReg RegR11);
        ];
      context.stack <- context.stack - 2
    )
  | (StmtReturn (ExprSwitch (expr, branches), _), _) ->
    compile_switch expr branches
  | (StmtReturn (ExprCall ((ExprVar label, _), args), _), _) ->
    if compile_intrinsic label args then (
      compile_return ()
    ) else (
      compile_call_args arg_regs args;
      match Hashtbl.find_opt context.vars label with
      | None ->
        (
          if context.stack <> 0 then (
            append_inst (InstDrop context.stack)
          );
          append_inst (InstJmp (OpLabel label))
        )
      | Some n ->
        (
          append_inst (InstMov (OpReg RegR10, get_var n));
          if context.stack <> 0 then (
            append_inst (InstDrop context.stack)
          );
          append_inst (InstJmp (OpReg RegR10))
        )
    )
  | (StmtReturn (ExprCall (expr, args), _), _) ->
    (
      compile_call_args arg_regs args;
      compile_expr expr;
      append_inst (InstPop (OpReg RegR10));
      context.stack <- context.stack - 1;
      if context.stack <> 0 then (
        append_inst (InstDrop context.stack)
      );
      append_inst (InstJmp (OpReg RegR10))
    )
  | (StmtReturn expr, _) ->
    (
      compile_expr expr;
      compile_return ()
    )

and compile_stmts : Parse.stmt_pos list -> bool =
  function
  | [] -> false
  | [(StmtReturn _, _) as stmt] ->
    (
      compile_stmt stmt;
      true
    )
  | (StmtReturn _, _) :: _ -> assert false
  | stmt :: rest ->
    (
      compile_stmt stmt;
      compile_stmts rest
    )

let compile_func (func : Parse.func) : unit =
  context.stack <- 0;
  Hashtbl.clear context.vars;
  append_inst (InstLabel (fst func.label));
  compile_func_args arg_regs func.args;
  assert (compile_stmts func.body)

let rec opt_push_pop (prev : inst list) : inst list -> inst list =
  function
  | [] -> List.rev prev
  | InstPush op_push :: InstPop op_pop :: insts when op_push = op_pop ->
    opt_push_pop prev insts
  | InstPush op_push :: InstPop op_pop :: insts ->
    opt_push_pop (InstMov (op_pop, op_push) :: prev) insts
  | InstPush _ :: InstDrop 1 :: insts -> opt_push_pop prev insts
  | inst :: insts -> opt_push_pop (inst :: prev) insts

let rec opt_jump (prev : inst list) : inst list -> inst list =
  function
  | [] -> List.rev prev
  | InstJmp (OpLabel label0) :: ((InstLabel label1 :: _) as insts)
    when label0 = label1 -> opt_jump prev insts
  | inst :: insts -> opt_jump (inst :: prev) insts

let compile (funcs : Parse.func Queue.t) : Buffer.t =
  Queue.iter compile_func funcs;
  let buffer : Buffer.t = Buffer.create 1024 in
  Buffer.add_string buffer
    "format ELF64\n\
     public entry_\n";
  Hashtbl.iter
    (fun k _ -> Buffer.add_string buffer (Printf.sprintf "extrn %s\n" k))
    context.externs;
  Queue.to_seq context.insts
  |> List.of_seq
  |> opt_push_pop []
  |> opt_jump []
  |> List.iter (fun inst -> Buffer.add_string buffer (show_inst inst));
  if
    (Hashtbl.length context.strings <> 0) ||
    (Queue.length context.tables <> 0)
  then (
    Buffer.add_string buffer "section '.rodata'\n";
    Hashtbl.iter
      (fun k v -> Buffer.add_string buffer (compile_string v k))
      context.strings;
    Queue.iter
      (fun t -> Buffer.add_string buffer (compile_table t))
      context.tables
  );
  buffer

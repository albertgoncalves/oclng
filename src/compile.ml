open Types

type reg =
  | RegRdi
  | RegRsi
  | RegRdx
  | RegRcx
  | RegR8
  | RegR9
  | RegR10
  | RegR10b
  | RegR11
  | RegEax
  | RegRax
  | RegRsp

type op =
  | OpReg of reg
  | OpDerefRegImm of (reg * int)
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
  | InstAnd of (op * op)
  | InstXor of (op * op)
  | InstLabel of string
  | InstCall of op
  | InstJmp of op
  | InstCmp of (op * op)
  | InstSete of op
  | InstRet

type context =
  {
    mutable k : int;
    mutable stack : int;
    mutable base : int;
    mutable vars : (string, int) Hashtbl.t;
    insts : inst Queue.t;
    strings : (string, string) Hashtbl.t;
    tables : (string * (string list)) Queue.t;
    externs : (string, unit) Hashtbl.t;
    mutable entry_found : bool;
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
    entry_found = false;
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
  | RegRsi -> "rsi"
  | RegRdx -> "rdx"
  | RegRcx -> "rcx"
  | RegR8 -> "r8"
  | RegR9 -> "r9"
  | RegR10 -> "r10"
  | RegR10b -> "r10b"
  | RegR11 -> "r11"
  | RegEax -> "eax"
  | RegRax -> "rax"
  | RegRsp -> "rsp"

let show_op : op -> string =
  function
  | OpReg reg -> show_reg reg
  | OpDerefRegImm (reg, 0) -> Printf.sprintf "qword [%s]" (show_reg reg)
  | OpDerefRegImm (reg, offset) when offset < 0 ->
    Printf.sprintf "qword [%s - %d]" (show_reg reg) (offset * -8)
  | OpDerefRegImm (reg, offset) ->
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
  | InstAnd (l, r) -> Printf.sprintf "\tand %s, %s\n" (show_op l) (show_op r)
  | InstXor (l, r) -> Printf.sprintf "\txor %s, %s\n" (show_op l) (show_op r)
  | InstLabel label -> Printf.sprintf "%s:\n" label
  | InstCall op -> Printf.sprintf "\tcall %s\n" (show_op op)
  | InstJmp op -> Printf.sprintf "\tjmp %s\n" (show_op op)
  | InstCmp (l, r) -> Printf.sprintf "\tcmp %s, %s\n" (show_op l) (show_op r)
  | InstSete op -> Printf.sprintf "\tsete %s\n" (show_op op)
  | InstRet -> "\tret\n"

let string_label : int -> string =
  Printf.sprintf "_str_%d_"

let append_inst (inst : inst) : unit =
  Queue.add inst context.insts

let append_insts : inst list -> unit =
  List.iter append_inst

let get_var (n : int) : op =
  OpDerefRegImm (RegRsp, context.stack - n)

let compile_string (str : string) (label : string) : string =
  Printf.sprintf "\t%s db %s,0\n" label (show_string str)

let compile_table ((table, branches) : (string * string list)) : string =
  Printf.sprintf "\t%s dq %s\n" table (String.concat "," branches)

let append_var (var : string) : unit =
  assert (not (Hashtbl.mem context.vars var));
  Hashtbl.add context.vars var context.stack

let rec compile_func_args (regs : reg list) : string list -> unit =
  function
  | [] -> ()
  | arg :: args ->
    (match regs with
     | [] -> assert false
     | reg :: regs ->
       (
         append_inst (InstPush (OpReg reg));
         context.stack <- context.stack + 1;
         append_var arg;
         compile_func_args regs args
       ))

let rec compile_expr : expr -> unit =
  function
  | ExprInt n ->
    (
      append_inst (InstPush (OpImm n));
      context.stack <- context.stack + 1
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
      append_inst (InstPush (OpLabel label));
      context.stack <- context.stack + 1
    )
  | ExprVar var ->
    (
      append_inst
        (match Hashtbl.find_opt context.vars var with
         | Some n -> InstPush (get_var n)
         | None -> InstPush (OpLabel var));
      context.stack <- context.stack + 1
    )
  | ExprCall (label, args) -> compile_call label args
  | ExprSwitch (expr, branches) -> compile_switch expr branches

and compile_call_args (regs : reg list) : expr list -> unit =
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

and compile_call (label : string) (args : expr list) : unit =
  match label with
  | "=" | "+" | "-" ->
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
            | _ -> assert false);
         append_inst (InstPush (OpReg RegR10));
         context.stack <- context.stack - 1
       )
     | _ -> assert false)
  | "printf" ->
    (
      compile_call_args arg_regs args;
      append_insts
        [
          InstXor (OpReg RegEax, OpReg RegEax);
          InstCall (OpLabel "printf");
          InstPush (OpReg RegRax);
        ];
      context.stack <- context.stack + 1;
      Hashtbl.replace context.externs "printf" ();
    )
  | _ ->
    (
      compile_call_args arg_regs args;
      append_insts
        [
          InstCall
            (match Hashtbl.find_opt context.vars label with
             | None -> OpLabel label
             | Some n -> get_var n);
          InstPush (OpReg RegRax);
        ];
      context.stack <- context.stack + 1;
    )

and compile_branch (label_end : string) (stmts : stmt list) : (string * bool) =
  let label_branch : string = Printf.sprintf "_branch_%d_" (get_k ()) in
  append_inst (InstLabel label_branch);
  let base : int = context.base in
  context.base <- context.stack;
  let vars : (string, int) Hashtbl.t = Hashtbl.copy context.vars in
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
  context.vars <- vars;
  (label_branch, returned)

and compile_switch (expr : expr) (branches : stmt list list) : unit =
  let label_table : string = Printf.sprintf "_table_%d_" (get_k ()) in
  let label_end : string = Printf.sprintf "_end_%d_" (get_k ()) in
  compile_expr expr;
  append_insts
    [
      InstPop (OpReg RegR10);
      InstJmp (OpDerefLabelReg (label_table, RegR10));
    ];
  context.stack <- context.stack - 1;
  let (label_branches, returns) : (string list * bool list) =
    List.split (List.map (compile_branch label_end) branches) in
  if not (List.for_all (fun x -> x) returns) then (
    append_insts
      [
        InstLabel label_end;
        InstPush (OpReg RegRax);
      ];
    context.stack <- context.stack + 1;
  );
  Queue.add (label_table, label_branches) context.tables

and compile_return (expr : expr) : unit =
  compile_expr expr;
  append_inst (InstPop (OpReg RegRax));
  context.stack <- context.stack - 1;
  if context.stack <> 0 then (
    append_inst (InstDrop context.stack)
  );
  append_inst InstRet

and compile_stmt : stmt -> unit =
  function
  | StmtHold expr -> compile_expr expr
  | StmtDrop expr ->
    (
      compile_expr expr;
      append_inst (InstDrop 1);
      context.stack <- context.stack - 1
    )
  | StmtLet (var, expr) ->
    (
      compile_expr expr;
      append_var var;
    )
  | StmtReturn (ExprCall ("=", _) as expr)
  | StmtReturn (ExprCall ("+", _) as expr)
  | StmtReturn (ExprCall ("-", _) as expr) -> compile_return expr
  | StmtReturn (ExprSwitch (expr, branches)) -> compile_switch expr branches
  | StmtReturn (ExprCall (label, args)) ->
    (
      compile_call_args arg_regs args;
      if context.stack <> 0 then (
        append_inst (InstDrop context.stack)
      );
      append_inst (InstJmp (OpLabel label))
    )
  | StmtReturn expr -> compile_return expr

and compile_stmts : stmt list -> bool =
  function
  | [] -> false
  | [StmtReturn _ as stmt] ->
    (
      compile_stmt stmt;
      true
    )
  | StmtReturn _ :: _ -> assert false
  | stmt :: rest ->
    (
      compile_stmt stmt;
      compile_stmts rest
    )

let compile_func (func : func) : unit =
  context.stack <- 0;
  Hashtbl.clear context.vars;
  (match func.label with
   | "entry" ->
     (
       context.entry_found <- true;
       append_inst (InstLabel "_entry_")
     )
   | label -> append_inst (InstLabel label));
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
  | InstJmp (OpLabel label0) :: InstLabel label1 :: insts
    when label0 = label1 -> opt_jump (InstLabel label1 :: prev) insts
  | inst :: insts -> opt_jump (inst :: prev) insts

let compile (funcs : func list) : Buffer.t =
  List.iter (fun f -> Printf.fprintf stderr "%s\n" (show_func f)) funcs;
  List.iter compile_func funcs;
  assert (context.entry_found);
  let buffer : Buffer.t = Buffer.create 1024 in
  Buffer.add_string buffer
    "format ELF64\n\
     public _entry_\n";
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
      (fun k v -> Buffer.add_string buffer (compile_string k v))
      context.strings;
    Queue.iter
      (fun t -> Buffer.add_string buffer (compile_table t))
      context.tables
  );
  buffer

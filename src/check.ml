type context =
  {
    mutable k : int;
    bindings : (string, Parse.type_pos) Hashtbl.t;
    funcs : (string, string list) Hashtbl.t;
    vars : string Queue.t;
    scopes : string Stack.t Stack.t;
    generics : string Stack.t;
    mutable func_label : string;
  }

let context : context =
  {
    k = 0;
    bindings = Hashtbl.create 64;
    funcs = Hashtbl.create 32;
    vars = Queue.create ();
    scopes = Stack.create ();
    generics = Stack.create ();
    func_label = "";
  }

let get_k () : int =
  let k : int = context.k in
  context.k <- context.k + 1;
  k

let get_var () : Parse.type' =
  let label : string = Printf.sprintf "_%d_" (get_k ()) in
  Queue.add label context.vars;
  TypeVar label

let rec deref : Parse.type_pos -> Parse.type_pos =
  function
  | (TypeStruct struct', position) ->
    (fst (Hashtbl.find context.bindings struct'), position)
  | (TypeVar var, position) as type' ->
    (match Hashtbl.find_opt context.bindings var with
     | Some type' -> deref type'
     | None -> type')
  | type' -> type'

let print_bindings () : unit =
  if context.func_label = "" then
    Printf.fprintf stderr "{\n"
  else
    Printf.fprintf stderr "%s {\n" context.func_label;
  Hashtbl.iter
    (fun label type' ->
       Printf.fprintf
         stderr
         "    %-12s : %s\n"
         label
         (Parse.show_type (fst type')))
    context.bindings;
  Printf.fprintf stderr "}\n\n"

let print_graph
    (graph : (string, (string, unit) Hashtbl.t) Hashtbl.t) : unit =
  Hashtbl.iter
    (fun parent children ->
       Printf.fprintf stderr "%-12s[ " parent;
       Hashtbl.iter
         (fun child _ -> Printf.fprintf stderr "%s " child)
         children;
       Printf.fprintf stderr "]\n")
    graph;
  Printf.fprintf stderr "\n"

let rec match_or_exit
    (expected : Parse.type')
    (given : Parse.type_pos) : unit =
  match (expected, deref given) with
  | (_, (_, None)) -> assert false
  | (expected, (given, _)) when expected = given -> ()
  | (TypeGeneric var, given) ->
    (match Hashtbl.find_opt context.bindings var with
     | Some (existing, _) -> match_or_exit existing given
     | None ->
       (
         Stack.push var context.generics;
         Hashtbl.add context.bindings var given
       ))
  | (_, (TypeGeneric _, _)) -> assert false
  | (TypeFunc (args0, ret0), (TypeFunc (args1, ret1), Some position)) ->
    (
      let args0_len : int = List.length args0 in
      let args1_len : int = List.length args1 in
      if args0_len <> args1_len then (
        Io.exit_at
          position
          (Printf.sprintf
             "expected %d arguments, given %d"
             args0_len
             args1_len)
      );
      List.iter
        (fun (a0, a1) -> match_or_exit a0 (a1, Some position))
        (List.combine args0 args1);
      match_or_exit ret0 (ret1, Some position)
    )
  | (TypeHeap items0, (TypeHeap items1, Some position)) ->
    (
      let items0_len : int = List.length items0 in
      let items1_len : int = List.length items1 in
      if items0_len <> items1_len then (
        Io.exit_at
          position
          (Printf.sprintf
             "expected %d arguments, given %d"
             items0_len
             items1_len)
      );
      List.iter
        (fun (i0, i1) -> match_or_exit i0 (i1, Some position))
        (List.combine items0 items1)
    )
  | (TypeVar var, given) ->
    (match Hashtbl.find_opt context.bindings var with
     | Some (existing, _) -> match_or_exit existing given
     | None -> Hashtbl.add context.bindings var given)
  | (TypeStruct struct', given) ->
    match_or_exit (fst (Hashtbl.find context.bindings struct')) given
  | (expected, (TypeVar var, position)) ->
    (match Hashtbl.find_opt context.bindings var with
     | Some existing -> match_or_exit expected existing
     | None -> Hashtbl.add context.bindings var (expected, position))
  | (_, (given, Some position)) ->
    Io.exit_at
      position
      (Printf.sprintf
         "expected `%s`, given `%s`"
         (Parse.show_type expected)
         (Parse.show_type given))

let rec swap
    (target : string)
    (replacement : Parse.type')
    (existing : Parse.type') : Parse.type' =
  match existing with
  | TypeVar var when var = target -> replacement
  | TypeFunc (args, return) ->
    TypeFunc
      (List.map (swap target replacement) args, swap target replacement return)
  | TypeHeap items -> TypeHeap (List.map (swap target replacement) items)
  | _ -> existing

let rec get_generic (vars : string list option) : Parse.type' -> Parse.type' =
  function
  | TypeVar var as type'->
    (match vars with
     | None -> TypeGeneric var
     | Some vars when List.mem var vars -> TypeGeneric var
     | _ -> type')
  | TypeFunc (args, return) ->
    TypeFunc (List.map (get_generic vars) args, get_generic vars return)
  | TypeHeap items -> TypeHeap (List.map (get_generic vars) items)
  | type' -> type'

let resolve () : unit =
  let remaining : string Queue.t = Queue.create () in
  while not (Queue.is_empty context.vars) do
    let label : string = Queue.take context.vars in
    match Hashtbl.find_opt context.bindings label with
    | Some (type', _) ->
      (
        Hashtbl.remove context.bindings label;
        Hashtbl.to_seq context.bindings
        |> List.of_seq
        |> List.iter
          (fun (binding, (current, position)) ->
             Hashtbl.replace
               context.bindings
               binding
               (swap label type' current, position))
      )
    | None -> Queue.add label remaining
  done;
  Queue.transfer remaining context.vars

let create_scope () : unit =
  Stack.push (Stack.create ()) context.scopes

let destroy_scope () : unit =
  resolve ();
  let scope : string Stack.t = Stack.pop context.scopes in
  while not (Stack.is_empty scope) do
    Hashtbl.remove context.bindings (Stack.pop scope)
  done

let rec find_vars (vars : string list) : Parse.type' -> string list =
  function
  | TypeVar var -> var :: vars
  | TypeFunc (args, return) ->
    List.rev_append
      (List.concat_map (find_vars []) args)
      (find_vars vars return)
  | TypeHeap items ->
    List.rev_append (List.concat_map (find_vars []) items) vars
  | _ -> vars

let rec walk_expr : Parse.expr_pos -> Parse.type_pos option =
  function
  | (ExprInt _, position) -> Some (TypeInt, Some position)
  | (ExprIndex (_, l, r), position) -> Some (TypeRange (l, r), Some position)
  | (ExprStr _, position) -> Some (TypeStr, Some position)
  | (ExprVar var, position) ->
    (match Hashtbl.find_opt context.bindings var with
     | None ->
       Io.exit_at position (Printf.sprintf "`%s` used before declaration" var)
     | type' -> type')

  | (ExprCall ((ExprVar "alloc", _), [(ExprInt n, _)]), position) ->
    Some (TypeHeap (List.init n (fun _ -> get_var ())), Some position)
  | (ExprCall ((ExprVar "alloc", _), _), _) -> assert false
  | (ExprCall ((ExprVar "child+", _), [expr; (ExprInt n, _)]), position) ->
    (match walk_expr expr with
     | Some (TypeHeap items, _) ->
       (match List.nth_opt items n with
        | Some type' ->
          (match deref (type', Some position) with
           | (TypeHeap _, _) -> None
           | _ -> assert false)
        | None ->
          Io.exit_at
            position
            (Printf.sprintf
               "index %d is out-of-bounds for allocation of length %d"
               n
               (List.length items)))
     | _ -> assert false)
  | (ExprCall ((ExprVar "deref", _), [expr; (ExprInt n, _)]), position) ->
    (match walk_expr expr with
     | Some type' ->
       (match deref type' with
        | (TypeHeap items, _) ->
          (match List.nth_opt items n with
           | None ->
             Io.exit_at
               position
               (Printf.sprintf
                  "index %d is out-of-bounds for allocation of length %d"
                  n
                  (List.length items))
           | Some type' -> Some (type', Some position))
        | _ ->
          Io.exit_at
            position
            (Printf.sprintf
               "unable to de-reference unknown allocation `%s`"
               (Parse.show_expr (fst expr))))
     | _ -> assert false)

  | (ExprCall (expr, arg_exprs), position) -> walk_call expr arg_exprs position
  | (ExprSwitch (expr, branches), position) ->
    walk_switch expr branches position
  | (ExprFunc func, _) -> Some (Hashtbl.find context.bindings (fst func.label))

and walk_call
    (expr : Parse.expr_pos)
    (arg_exprs : Parse.expr_pos list)
    (position : Io.position) : Parse.type_pos option =
  match walk_expr expr with
  | Some (TypeAny, _) as type' ->
    (
      let _ : Parse.type_pos option list = List.map walk_expr arg_exprs in
      type'
    )
  | Some (TypeFunc (arg_types, return), _) ->
    (
      let arg_exprs_len : int = List.length arg_exprs in
      let arg_types_len : int = List.length arg_types in
      if arg_types_len <> arg_exprs_len then (
        Io.exit_at
          position
          (Printf.sprintf
             "`%s` takes %d argument(s) but %d were provided"
             (Parse.show_expr_pos expr)
             arg_types_len
             arg_exprs_len)
      );
      let n : int = Stack.length context.generics in
      let expr_types : Parse.type_pos list =
        List.filter_map walk_expr arg_exprs in
      assert (arg_exprs_len = (List.length expr_types));
      List.iter
        (fun (a, b) -> match_or_exit a b)
        (List.combine arg_types expr_types);
      let return : Parse.type_pos =
        match return with
        | TypeGeneric var -> Hashtbl.find context.bindings var
        | _ -> (return, Some position) in
      while n < (Stack.length context.generics) do
        Hashtbl.remove context.bindings (Stack.pop context.generics)
      done;
      Some return
    )
  | Some (TypeVar var, _) ->
    (
      let arg_types : Parse.type_pos list =
        List.filter_map walk_expr arg_exprs in
      let arg_exprs_len : int = List.length arg_exprs in
      let arg_types_len : int = List.length arg_types in
      if arg_types_len <> arg_exprs_len then (
        Io.exit_at
          position
          (Printf.sprintf
             "expected %d arguments, given %d"
             arg_types_len
             arg_exprs_len)
      );
      let return : Parse.type' = get_var () in
      match Hashtbl.find_opt context.bindings var with
      | Some _ -> assert false
      | None ->
        (
          Hashtbl.add
            context.bindings
            var
            (TypeFunc (List.map fst arg_types, return), Some position);
          Some (return, Some position)
        )
    )
  | Some (type', _) ->
    Io.exit_at
      position
      (Printf.sprintf "function has type `%s`" (Parse.show_type type'))
  | _ -> assert false

and walk_switch
    (expr : Parse.expr_pos)
    (branches : Parse.stmt_pos list list)
    (position : Io.position) : Parse.type_pos option =
  let type' : Parse.type_pos =
    match expr with
    | (ExprCall ((ExprVar "%", _), [expr; (ExprInt n, _)]), position)
      when 0 < n ->
      (match walk_expr expr with
       | Some type' ->
         (
           match_or_exit TypeInt type';
           (TypeRange (0, n - 1), Some position)
         )
       | _ -> assert false)
    | (ExprInt n, position) when 0 <= n -> (TypeRange (0, n), Some position)
    | _ ->
      (match walk_expr expr with
       | Some type' -> type'
       | None -> assert false) in
  match_or_exit (TypeRange (0, List.length branches - 1)) type';
  let branches : Parse.type_pos list =
    List.concat_map
      (fun branch ->
         create_scope ();
         let type' : Parse.type_pos list = List.filter_map walk_stmt branch in
         destroy_scope ();
         type')
      branches in
  match branches with
  | (type', _) :: types ->
    if List.for_all ((=) type') (List.map fst types) then
      Some (type', Some position)
    else
      assert false
  | [] -> None

and walk_stmt : Parse.stmt_pos -> Parse.type_pos option =
  function
  | (StmtDrop expr, _) ->
    (
      let _ : Parse.type_pos option = walk_expr expr in
      None
    )
  | (StmtHold expr, _) -> walk_expr expr
  | (StmtReturn expr, _) ->
    (match Hashtbl.find context.bindings context.func_label with
     | (TypeFunc (args, return_type), Some position) ->
       (
         (match walk_expr expr with
          | Some type' -> match_or_exit return_type type'
          | None -> ());
         None
       )
     | _ -> assert false)
  | (StmtLet (var, expr), position) ->
    (match walk_expr expr with
     | Some (TypeAny, _) -> assert false
     | Some type' ->
       (match Hashtbl.find_opt context.bindings var with
        | None ->
          (
            let scope : string Stack.t = Stack.pop context.scopes in
            Stack.push var scope;
            Stack.push scope context.scopes;
            Hashtbl.add context.bindings var type';
            None
          )
        | Some _ ->
          Io.exit_at
            position
            (Printf.sprintf "`%s` shadows existing variable binding" var))
     | None -> assert false)
  | ((StmtSetLocal (var, expr)), position) ->
    (match Hashtbl.find_opt context.bindings var with
     | None ->
       Io.exit_at
         position
         (Printf.sprintf "variable `%s` not yet defined" var)
     | Some (var_type, _) ->
       (match walk_expr expr with
        | Some expr_type ->
          (
            match_or_exit var_type expr_type;
            None
          )
        | None -> assert false))
  | (StmtSetHeap (var, n, value), position) ->
    (match walk_expr var with
     | Some type' ->
       (match deref type' with
        | (TypeHeap items, _) ->
          (match List.nth_opt items n with
           | Some item ->
             (match walk_expr value with
              | Some value_type ->
                (
                  match_or_exit item value_type;
                  None
                )
              | None -> assert false)
           | None ->
             Io.exit_at
               position
               (Printf.sprintf
                  "index %d is out-of-bounds for allocation of length %d"
                  n
                  (List.length items)))
        | _ ->
          Io.exit_at
            position
            (Printf.sprintf
               "unable to write to unknown allocation `%s`"
               (Parse.show_expr (fst var))))
     | None -> assert false)

and walk_func (func : Parse.func) : unit =
  create_scope ();
  let (label, position) : string * Io.position = func.label in
  context.func_label <- label;
  (match (
     Hashtbl.find_opt context.funcs label,
     Hashtbl.find_opt context.bindings label
   ) with
   | (Some arg_labels, Some (TypeFunc (arg_types, _), position)) ->
     List.combine arg_labels arg_types
     |> List.iter
       (fun (arg, type') ->
          assert (not (Hashtbl.mem context.bindings arg));
          Hashtbl.add context.bindings arg (type', position);
          let scope : string Stack.t = Stack.pop context.scopes in
          Stack.push arg scope;
          Stack.push scope context.scopes)
   | _ -> assert false);
  let _ : Parse.type_pos option list = List.map walk_stmt func.body in
  print_bindings ();
  destroy_scope ();
  (match Hashtbl.find context.bindings context.func_label with
   | (TypeFunc (args, return), position) ->
     Hashtbl.replace
       context.bindings
       context.func_label
       (
         TypeFunc
           (
             List.map (get_generic None) args,
             get_generic (Some (List.concat_map (find_vars []) args)) return
           ),
         position
       )
   | _ -> assert false)

let set_intrinsic (label : string) (type' : Parse.type') : unit =
  assert (not (Hashtbl.mem context.bindings label));
  Hashtbl.add context.bindings label (type', None)

let collect (funcs : Parse.func Queue.t) : unit =
  let locals : Parse.func Queue.t = Queue.create () in
  let rec walk_expr (expr : Parse.expr_pos) : unit =
    match fst expr with
    | Parse.ExprInt _
    | Parse.ExprIndex _
    | Parse.ExprStr _
    | Parse.ExprVar _ -> ()
    | Parse.ExprFunc func ->
      (
        Queue.add func locals;
        List.iter walk_stmt func.body
      )
    | Parse.ExprCall (expr, args) ->
      (
        walk_expr expr;
        List.iter walk_expr args
      )
    | Parse.ExprSwitch (expr, branches) ->
      (
        walk_expr expr;
        List.iter (List.iter walk_stmt) branches
      )
  and walk_stmt (stmt : Parse.stmt_pos) : unit =
    match fst stmt with
    | Parse.StmtDrop expr
    | Parse.StmtHold expr
    | Parse.StmtReturn expr
    | Parse.StmtLet (_, expr)
    | Parse.StmtSetLocal (_, expr) -> walk_expr expr
    | Parse.StmtSetHeap (var, _, value) ->
      (
        walk_expr var;
        walk_expr value
      ) in
  Queue.iter (fun (func : Parse.func) -> List.iter walk_stmt func.body) funcs;
  Queue.transfer locals funcs

let prepare (func : Parse.func) : unit =
  let (label, position) : string * Io.position = func.label in
  (match Hashtbl.find_opt context.bindings label with
   | None -> ()
   | Some _ ->
     Io.exit_at
       position
       (Printf.sprintf "function `%s` is already defined" label));
  let args : (string * Parse.type') list =
    List.map
      (fun (arg, type', position) ->
         match type' with
         | None -> (arg, get_var ())
         | Some type' -> (arg, type'))
      func.args in
  assert (not (Hashtbl.mem context.funcs label));
  Hashtbl.add context.funcs label (List.map fst args);
  assert (not (Hashtbl.mem context.bindings label));
  Hashtbl.add
    context.bindings
    label
    (TypeFunc (List.map snd args, get_var ()), Some position)

let reorder (funcs : Parse.func Queue.t) : unit =
  let graph : (string, (string, unit) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 16 in
  Queue.iter
    (fun (func : Parse.func) ->
       Hashtbl.add graph (fst func.label) (Hashtbl.create 8))
    funcs;
  let rec walk_expr (parent : string) (expr : Parse.expr_pos) : unit =
    match fst expr with
    | Parse.ExprInt _
    | Parse.ExprIndex _
    | Parse.ExprStr _ -> ()
    | Parse.ExprVar var ->
      if Hashtbl.mem graph var then (
        Hashtbl.replace (Hashtbl.find graph parent) var ()
      )
    | Parse.ExprFunc func ->
      Hashtbl.replace (Hashtbl.find graph parent) (fst func.label) ()
    | Parse.ExprCall (expr, args) ->
      (
        walk_expr parent expr;
        List.iter (walk_expr parent) args
      )
    | Parse.ExprSwitch (expr, branches) ->
      (
        walk_expr parent expr;
        List.iter (List.iter (walk_stmt parent)) branches
      )
  and walk_stmt (parent : string) (stmt : Parse.stmt_pos) : unit =
    match fst stmt with
    | Parse.StmtDrop expr
    | Parse.StmtHold expr
    | Parse.StmtReturn expr
    | Parse.StmtLet (_, expr)
    | Parse.StmtSetLocal (_, expr) -> walk_expr parent expr
    | Parse.StmtSetHeap (var, _, value) ->
      (
        walk_expr parent var;
        walk_expr parent value
      ) in
  Queue.iter
    (fun (func : Parse.func) ->
       List.iter (walk_stmt (fst func.label)) func.body)
    funcs;
  print_graph graph;
  let ordering : string Queue.t =  Queue.create () in
  let parents : (string * (string, unit) Hashtbl.t) Queue.t =
    Queue.of_seq (Hashtbl.to_seq graph) in
  let visited : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  while not (Queue.is_empty parents) do
    let (parent, children) : (string * (string, unit) Hashtbl.t) =
      Queue.take parents in
    if ((Hashtbl.length children) = 0) || (Hashtbl.mem visited parent) then (
      Queue.add parent ordering;
      Queue.iter (fun (_, children) -> Hashtbl.remove children parent) parents;
      Hashtbl.clear visited
    ) else (
      Queue.add (parent, children) parents;
      Hashtbl.add visited parent ()
    )
  done;
  let mapping : (string, Parse.func) Hashtbl.t = Hashtbl.create 16 in
  while not (Queue.is_empty funcs) do
    let func : Parse.func = Queue.take funcs in
    Hashtbl.add mapping (fst func.label) func
  done;
  Queue.iter
    (fun label -> Queue.add (Hashtbl.find mapping label) funcs)
    ordering

let check (funcs : Parse.func Queue.t) (structs : Parse.structs) : unit =
  Hashtbl.to_seq structs
  |> Seq.iter
    (fun (struct', type') ->
       assert (not (Hashtbl.mem context.bindings struct'));
       Hashtbl.add context.bindings struct' type');
  set_intrinsic "printf" TypeAny;
  set_intrinsic "print_stack" (TypeFunc ([], TypeAny));
  set_intrinsic "free" (TypeFunc ([], TypeAny));
  set_intrinsic "=" (TypeFunc ([TypeInt; TypeInt], TypeRange (0, 1)));
  set_intrinsic "+" (TypeFunc ([TypeInt; TypeInt], TypeInt));
  set_intrinsic "-" (TypeFunc ([TypeInt; TypeInt], TypeInt));
  set_intrinsic "*" (TypeFunc ([TypeInt; TypeInt], TypeInt));
  set_intrinsic "/" (TypeFunc ([TypeInt; TypeInt], TypeInt));
  set_intrinsic "%" (TypeFunc ([TypeInt; TypeInt], TypeInt));
  collect funcs;
  Queue.iter prepare funcs;
  reorder funcs;
  Queue.iter walk_func funcs;
  assert ((Stack.length context.generics) = 0);
  match Hashtbl.find_opt context.bindings "entry_" with
  | Some type' -> match_or_exit (TypeFunc ([], TypeInt)) type'
  | _ -> Io.exit_at (Io.position_at (Io.context.len - 1)) "`entry` not defined"

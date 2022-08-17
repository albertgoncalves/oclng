type type' =
  | TypeAny
  | TypeVar of string
  | TypeRange of (int * int)
  | TypeInt
  | TypeStr
  | TypeFunc of ((type' list) * type')
  | TypeHeap of type' list
  | TypeGeneric of string

and type_pos = (type' * (Io.position option))

let rec show_type : type' -> string =
  function
  | TypeAny -> "any"
  | TypeVar var -> var
  | TypeRange (l, r) -> Printf.sprintf "[%d, %d]" l r
  | TypeInt -> "int"
  | TypeStr -> "str"
  | TypeFunc (args, return) ->
    Printf.sprintf "\\%s { %s }" (show_types args) (show_type return)
  | TypeHeap items -> Printf.sprintf "(%s)" (show_types items)
  | TypeGeneric var -> Printf.sprintf "'%s" var

and show_types (types : type' list) : string =
  String.concat " " (List.map show_type types)

type context =
  {
    mutable k : int;
    bindings : (string, type_pos) Hashtbl.t;
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

let get_var () : type' =
  let label : string = Printf.sprintf "_%d_" (get_k ()) in
  Queue.add label context.vars;
  TypeVar label

let rec deref : type_pos -> type_pos =
  function
  | (TypeVar var, position) as type' ->
    (match Hashtbl.find_opt context.bindings var with
     | Some type' -> deref (type')
     | None -> type')
  | type' -> type'

let print_bindings () : unit =
  Printf.fprintf stderr "%s {\n" context.func_label;
  Hashtbl.iter
    (fun label type' ->
       Printf.fprintf stderr "    %-8s : %s\n" label (show_type (fst type')))
    context.bindings;
  Printf.fprintf stderr "}\n\n"

let rec match_or_exit (expected : type') (given : type_pos) : unit =
  match (expected, deref given) with
  | (TypeGeneric var, given) ->
    (match Hashtbl.find_opt context.bindings var with
     | Some (existing, _) -> match_or_exit existing given
     | None ->
       (
         Stack.push var context.generics;
         Hashtbl.add context.bindings var given
       ))
  | (_ , (TypeGeneric _, _)) -> assert false
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
      List.combine args0 args1
      |> List.iter (fun (a0, a1) -> match_or_exit a0 (a1, Some position));
      match_or_exit ret0 (ret1, Some position)
    )
  | (TypeFunc _, (TypeFunc _, None)) -> assert false
  | (expected, (given, position)) when expected = given -> ()
  | (TypeVar var, given) ->
    (match Hashtbl.find_opt context.bindings var with
     | Some (existing, _) -> match_or_exit existing given
     | None -> Hashtbl.add context.bindings var given)
  | (expected, (TypeVar var, position)) ->
    (match Hashtbl.find_opt context.bindings var with
     | Some existing -> match_or_exit expected existing
     | None -> Hashtbl.add context.bindings var (expected, position))
  | (_, (_, Some position)) ->
    Io.exit_at
      position
      (Printf.sprintf
         "expected `%s`, given `%s`"
         (show_type expected)
         (show_type (fst given)))
  | _ -> assert false

let rec swap
    (target : string)
    (replacement : type')
    (existing : type') : type' =
  match existing with
  | TypeVar var when var = target -> replacement
  | TypeFunc (args, return) ->
    TypeFunc
      (List.map (swap target replacement) args, swap target replacement return)
  | TypeHeap items -> TypeHeap (List.map (swap target replacement) items)
  | _ -> existing

let rec get_generic : type' -> type' =
  function
  | TypeVar var -> TypeGeneric var
  | TypeFunc (args, return) ->
    TypeFunc (List.map get_generic args, get_generic return)
  | TypeHeap items -> TypeHeap (List.map get_generic items)
  | type' -> type'

let resolve () : unit =
  let remaining : string Queue.t = Queue.create () in
  while not (Queue.is_empty context.vars) do
    let label : string = Queue.pop context.vars in
    match Hashtbl.find_opt context.bindings label with
    | Some (type', _) ->
      (
        Hashtbl.remove context.bindings label;
        Hashtbl.to_seq context.bindings
        |> List.of_seq
        |> List.iter
          (fun (key, (current, position)) ->
             Hashtbl.replace
               context.bindings
               key
               (swap label type' current, position))
      )
    | None -> Queue.add label remaining
  done;
  Queue.transfer remaining context.vars

let patch ((type', pos0) : type_pos) (pos1 : Io.position) : type_pos =
  match pos0 with
  | Some position -> (type', pos0)
  | None -> (type', Some pos1)

let create_scope () : unit =
  Stack.push (Stack.create ()) context.scopes

let destroy_scope () : unit =
  resolve ();
  let scope : string Stack.t = Stack.pop context.scopes in
  while not (Stack.is_empty scope) do
    Hashtbl.remove context.bindings (Stack.pop scope)
  done

let prepare (func : Parse.func) : unit =
  let (label, position) : string * Io.position = func.label in
  (match Hashtbl.find_opt context.bindings label with
   | None -> ()
   | Some _ ->
     Io.exit_at
       position
       (Printf.sprintf "function `%s` is already defined" label));
  let args : (string * type') list =
    List.map (fun (arg, position) -> (arg, get_var ())) func.args in
  Hashtbl.add context.funcs label (List.map fst args);
  Hashtbl.add
    context.bindings
    label
    (TypeFunc (List.map snd args, get_var ()), Some position)

let rec find_vars (vars : string list) : type' -> string list =
  function
  | TypeVar var -> var :: vars
  | TypeFunc (args, return) ->
    List.rev_append
      (List.concat_map (find_vars []) args)
      (find_vars vars return)
  | TypeHeap items ->
    List.rev_append (List.concat_map (find_vars []) items) vars
  | _ -> vars

let rec walk_expr : Parse.expr_pos -> type_pos option =
  function
  | (ExprInt _, position) -> Some (TypeInt, Some position)
  | (ExprIndex (_, l, r), position) -> Some (TypeRange (l, r), Some position)
  | (ExprStr _, position) -> Some (TypeStr, Some position)
  | (ExprVar var, position) ->
    (match Hashtbl.find_opt context.bindings var with
     | None ->
       Io.exit_at position (Printf.sprintf "`%s` used before declaration" var)
     | type' -> type')
  | (ExprCall (expr, arg_exprs), position) -> walk_call expr arg_exprs position
  | (ExprSwitch (expr, branches), position) ->
    walk_switch expr branches position
  | (ExprFunc func, _) ->
    (
      let prev : string = context.func_label in
      prepare func;
      walk_func func;
      let current : string = context.func_label in
      context.func_label <- prev;
      Some (Hashtbl.find context.bindings current)
    )

and walk_call
    (expr : Parse.expr_pos)
    (arg_exprs : Parse.expr_pos list)
    (position : Io.position) : type_pos option =
  match walk_expr expr with
  | Some (TypeAny, _) as type' ->
    (
      let _ : type_pos option list = List.map walk_expr arg_exprs in
      type'
    )
  | Some (TypeFunc (arg_types, return), position) ->
    (
      let arg_exprs_len : int = List.length arg_exprs in
      let arg_types_len : int = List.length arg_types in
      if arg_types_len <> arg_exprs_len then (
        match position with
        | Some position ->
          Io.exit_at
            position
            (Printf.sprintf
               "`%s` takes %d argument(s) but %d were provided"
               (Parse.show_expr_pos expr)
               arg_types_len
               arg_exprs_len)
        | None -> assert false
      );
      let n : int = Stack.length context.generics in
      let expr_types : type_pos list = List.filter_map walk_expr arg_exprs in
      assert (arg_exprs_len = (List.length expr_types));
      List.iter
        (fun (a, b) ->
           match position with
           | Some position -> match_or_exit a (patch b position)
           | None -> match_or_exit a b)
        (List.combine arg_types expr_types);
      let return : type_pos =
        match return with
        | TypeGeneric var -> Hashtbl.find context.bindings var
        | _ -> (return, position) in
      while n < (Stack.length context.generics) do
        Hashtbl.remove context.bindings (Stack.pop context.generics)
      done;
      Some return
    )
  | Some (TypeVar var, position) ->
    (
      let arg_types : type_pos list = List.filter_map walk_expr arg_exprs in
      let arg_exprs_len : int = List.length arg_exprs in
      let arg_types_len : int = List.length arg_types in
      if arg_types_len <> arg_exprs_len then (
        match position with
        | Some position ->
          Io.exit_at
            position
            (Printf.sprintf
               "expected %d arguments, given %d"
               arg_types_len
               arg_exprs_len)
        | None -> assert false
      );
      let return : type' = get_var () in
      match Hashtbl.find_opt context.bindings var with
      | Some _ -> assert false
      | None ->
        (
          Hashtbl.add
            context.bindings
            var
            (TypeFunc (List.map fst arg_types, return), position);
          Some (return, position)
        )
    )
  | Some (type', _) ->
    Io.exit_at
      position
      (Printf.sprintf "function has type `%s`" (show_type type'))
  | _ -> assert false

and walk_switch
    (expr : Parse.expr_pos)
    (branches : Parse.stmt_pos list list)
    (position : Io.position) : type_pos option =
  let type' : type_pos =
    match expr with
    | (ExprCall ((ExprVar "%", _), [expr; (ExprInt n, _)]), position)
      when 0 < n ->
      (
        match walk_expr expr with
        | Some type' ->
          (
            match_or_exit TypeInt type';
            (TypeRange (0, n - 1), Some position)
          )
        | _ -> assert false
      )
    | (ExprInt n, position) when 0 <= n ->
      (TypeRange (0, n), Some position)
    | _ ->
      (match walk_expr expr with
       | Some type' -> type'
       | None -> assert false) in
  match_or_exit
    (TypeRange (0, List.length branches - 1))
    (patch type' position);
  let branches : type_pos list =
    List.concat_map
      (fun branch ->
         create_scope ();
         let type' : type_pos list = List.filter_map walk_stmt branch in
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

and walk_stmt : Parse.stmt_pos -> type_pos option =
  function
  | (StmtDrop expr, _) ->
    (
      let _ : type_pos option = walk_expr expr in
      None
    )
  | (StmtHold expr, _) -> walk_expr expr
  | (StmtReturn expr, _) ->
    (match Hashtbl.find context.bindings context.func_label with
     | (TypeFunc (args, return_type), Some position) ->
       (
         (match walk_expr expr with
          | Some type' -> match_or_exit return_type (patch type' position)
          | None -> ());
         None
       )
     | _ -> assert false)
  | (StmtLet (var, expr), position) ->
    (
      match walk_expr expr with
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
      | None -> assert false
    )
  | (StmtSetLocal _, _) -> assert false
  | (StmtSetHeap _, _) -> assert false

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
          Hashtbl.add context.bindings arg (type', position);
          let scope : string Stack.t = Stack.pop context.scopes in
          Stack.push arg scope;
          Stack.push scope context.scopes)
   | _ -> assert false);
  let _ : type_pos option list = List.map walk_stmt func.body in
  destroy_scope ();
  (
    match Hashtbl.find context.bindings context.func_label with
    | (TypeFunc (args, return), position) ->
      let vars : string list = List.concat_map (find_vars []) args in
      let return : type' =
        match return with
        | TypeVar var when List.mem var vars -> get_generic return
        | _ -> return in
      Hashtbl.replace
        context.bindings
        context.func_label
        (TypeFunc (List.map get_generic args, return), position)
    | _ -> assert false
  );
  print_bindings ()

let set_intrinsic (label : string) (type' : type') : unit =
  Hashtbl.add context.bindings label (type', None)

let check (funcs : Parse.func Queue.t) : unit =
  set_intrinsic "printf" TypeAny;
  set_intrinsic "=" (TypeFunc ([TypeInt; TypeInt], TypeRange (0, 1)));
  set_intrinsic "+" (TypeFunc ([TypeInt; TypeInt], TypeInt));
  set_intrinsic "-" (TypeFunc ([TypeInt; TypeInt], TypeInt));
  set_intrinsic "*" (TypeFunc ([TypeInt; TypeInt], TypeInt));
  set_intrinsic "/" (TypeFunc ([TypeInt; TypeInt], TypeInt));
  set_intrinsic "%" (TypeFunc ([TypeInt; TypeInt], TypeInt));
  Queue.iter prepare funcs;
  Queue.iter walk_func funcs;
  assert ((Stack.length context.generics) = 0);
  match Hashtbl.find_opt context.bindings "entry_" with
  | Some type' -> match_or_exit (TypeFunc ([], TypeInt)) type'
  | _ -> Io.exit_at (Io.position_at (Io.context.len - 1)) "`entry` not defined"

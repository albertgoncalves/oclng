type type' =
  | TypeVar of int
  | TypeRange of (int * int)
  | TypeInt
  | TypeStr
  | TypeFn of ((type_pos list) * (type_pos option))
  | TypeHeap of type_pos list option

and type_pos = (type' * Io.position)

let rec show_type : type_pos -> string =
  function
  | (TypeVar x, _) -> Printf.sprintf "_%d_" x
  | (TypeRange (l, r), _) -> Printf.sprintf "[%d, %d]" l r
  | (TypeInt, _) -> "int"
  | (TypeStr, _) -> "str"
  | (TypeFn (args, None), _) -> Printf.sprintf "\\%s { ? }" (show_types args)
  | (TypeFn (args, Some type'), _) ->
    Printf.sprintf "\\%s { ? }" (show_types args)
  | (TypeHeap None, _) -> "(?)"
  | (TypeHeap Some types, _) -> Printf.sprintf "(%s)" (show_types types)

and show_types (types : type_pos list) : string =
  String.concat " " (List.map show_type types)

type context =
  {
    mutable k : int;
    funcs : (string, type_pos) Hashtbl.t;
    bindings : (string, type_pos) Hashtbl.t;
  }

let context : context =
  {
    k = 0;
    funcs = Hashtbl.create 32;
    bindings = Hashtbl.create 64;
  }

let get_k () : int =
  let k : int = context.k in
  context.k <- context.k + 1;
  k

let walk_func (func : Parse.func) : unit =
  Hashtbl.clear context.bindings;
  let (label, position) : string * Io.position = func.label in
  (match Hashtbl.find_opt context.funcs label with
   | None -> ()
   | Some _ ->
     Io.exit_at
       position
       (Printf.sprintf "function `%s` is already defined" label));
  let args : (string * type_pos) list =
    List.map
      (fun (arg, position) -> (arg, (TypeVar (get_k ()), position)))
      func.args in
  Hashtbl.add context.funcs label (TypeFn (List.map snd args, None), position);
  List.iter (fun (arg, type') -> Hashtbl.add context.bindings arg type') args;
  ()

let check (funcs : Parse.func Queue.t) : unit =
  Queue.iter
    (fun func ->
       walk_func func;
       let (label, _) : Parse.string_pos = func.label in
       Printf.fprintf
         stderr
         "%s = %s\n"
         label
         (show_type (Hashtbl.find context.funcs label));
       Hashtbl.iter
         (fun label type' ->
            Printf.fprintf stderr "    %s = %s\n" label (show_type type'))
         context.bindings;
       Printf.fprintf stderr "\n")
    funcs;
  match Hashtbl.find_opt context.funcs "entry_" with
  | Some (TypeFn ([], Some (TypeInt, _)), _) -> ()
  | Some (_, position) ->
    Io.exit_at
      position
      "`entry` should take no arguments and return an integer value"
  | _ -> Io.exit_at (Io.position_at (Io.context.len - 1)) "`entry` not defined"

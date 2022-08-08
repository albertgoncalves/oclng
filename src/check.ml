type type' =
  | TypeVar of int
  | TypeRange of (int * int)
  | TypeInt
  | TypeStr
  | TypeFn of ((type_pos list) * type')
  | TypeHeap of type_pos list

and type_pos = (type' * Io.position)

let rec show_type : type' -> string =
  function
  | TypeVar x -> Printf.sprintf "_%d_" x
  | TypeRange (l, r) -> Printf.sprintf "[%d, %d]" l r
  | TypeInt -> "int"
  | TypeStr -> "str"
  | TypeFn (args, type') ->
    Printf.sprintf
      "\\%s { %s }"
      (show_types (List.map fst args))
      (show_type type')
  | TypeHeap types -> Printf.sprintf "(%s)" (show_types (List.map fst types))

and show_types (types : type' list) : string =
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
  Hashtbl.add
    context.funcs
    label
    (TypeFn (List.map snd args, TypeVar (get_k ())), position);
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
         (show_type (fst (Hashtbl.find context.funcs label)));
       Hashtbl.iter
         (fun label type' ->
            Printf.fprintf
              stderr
              "    %s = %s\n"
              label
              (show_type (fst type')))
         context.bindings;
       Printf.fprintf stderr "\n")
    funcs;
  match Hashtbl.find_opt context.funcs "entry_" with
  | Some (TypeFn ([], TypeInt), _) -> ()
  | Some (_, position) ->
    Io.exit_at
      position
      "`entry` should take no arguments and return an integer value"
  | _ -> Io.exit_at (Io.position_at (Io.context.len - 1)) "`entry` not defined"

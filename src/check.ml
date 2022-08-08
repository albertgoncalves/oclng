type type' =
  | TypeVar of string
  | TypeRange of (int * int)
  | TypeInt
  | TypeStr
  | TypeFunc of ((type' list) * type')
  | TypeHeap of type' list

and type_pos = (type' * Io.position)

let rec show_type : type' -> string =
  function
  | TypeVar x -> x
  | TypeRange (l, r) -> Printf.sprintf "[%d, %d]" l r
  | TypeInt -> "int"
  | TypeStr -> "str"
  | TypeFunc (args, type') ->
    Printf.sprintf "\\%s { %s }" (show_types args) (show_type type')
  | TypeHeap types -> Printf.sprintf "(%s)" (show_types types)

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

let get_var () : string =
  Printf.sprintf "_%d_" (get_k ())

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
      (fun (arg, position) -> (arg, (TypeVar (get_var ()), position)))
      func.args in
  Hashtbl.add
    context.funcs
    label
    (
      TypeFunc
        (List.map (fun x -> x |> snd |> fst) args, TypeVar (get_var ())),
      position
    );
  List.iter (fun (arg, type') -> Hashtbl.add context.bindings arg type') args;
  ()

let match_or_exit (expected : type') ((found, position) : type_pos) : unit =
  if found = expected then
    ()
  else
    Io.exit_at
      position
      (Printf.sprintf
         "expected `%s`, found `%s`"
         (show_type expected)
         (show_type found))

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
  | Some type' -> match_or_exit (TypeFunc ([], TypeInt)) type'
  | _ -> Io.exit_at (Io.position_at (Io.context.len - 1)) "`entry` not defined"

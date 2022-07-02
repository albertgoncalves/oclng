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

type branch = (string list) * (expr list)

and expr =
  | ExprDrop of expr
  | ExprRet of expr
  | ExprInt of int
  | ExprStr of string
  | ExprVar of string
  | ExprAssign of (string * expr)
  | ExprIfThen of (expr * expr list * expr list)
  | ExprBinOp of (bin_op * expr * expr)
  | ExprCall of (call * expr list)
  | ExprUnpack of (expr * branch list)

type func =
  {
    label : string;
    args : string list;
    body : expr list;
  }
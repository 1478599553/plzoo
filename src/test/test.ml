type symbol = string
and lambda = symbol * exp
and apply = exp * exp
and exp = 
  | Symbol of symbol
  | Lambda of lambda
  | Apply of apply
and closure = lambda * env
and env = (symbol * value) list
and value = 
  | Closure of closure

let rec interp (expr:exp) (envi:env) : value = 
  match expr with
  | Symbol(sym) -> lookup sym envi
  | Lambda(lam) -> Closure(lam,envi)
  | Apply(argu_exp,func_exp) -> 
    let argu = interp argu_exp envi in
    let func_exp = interp func_exp envi in
    match func_exp with
    | Closure((para,body),envir) -> interp body ((para,argu)::envir )
    
and lookup (sym:symbol) (envi:env):value = List.assoc sym envi

let rec show (expr:exp) : string = 
  match expr with
  | Symbol(s) -> s
  | Lambda(para,body) -> "λ" ^ para ^ "." ^ show body
  | Apply(argu,lam) -> "(" ^ show lam ^ " " ^ show argu ^ ")";;

(* λx.(x x) *)
let expr = Lambda("x",Apply(Symbol("x"),Symbol("x")))
let y = Apply(expr,expr)
let a = interp y []
let aexp = match a with
| Closure(lam,_) -> Lambda(lam)

let () = print_endline (show aexp)

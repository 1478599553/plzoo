module MiniHaskell = Zoo.Main(struct

  let name = "MiniHaskell"

  type command = Syntax.toplevel_cmd

  type environment = (string * Syntax.htype) list * Interpret.environment

  let print_depth = ref 100

  let options = [("-p", Arg.Int (fun n -> print_depth := n), "set print depth")]

  let initial_environment = ([], [])

  let file_parser = Some (Parser.file Lexer.token)

  let toplevel_parser = Some (Parser.toplevel Lexer.token)

  let exec (ctx, env) = function
    | Syntax.Expr e ->
      (* type check [e], evaluate, and print result *)
       let ty = Type_check.type_of ctx e in
       let v = Interpret.interp env e in
       Zoo.print_info "- : %s = " (Syntax.string_of_type ty) ;
       Interpret.print_result !print_depth v ;
       Zoo.print_info "@." ;
       (ctx, env)
    | Syntax.Def (x, e) ->
       (* type check [e], and store it unevaluated! *)
       let ty = Type_check.type_of ctx e in
       Zoo.print_info "val %s : %s@." x (Syntax.string_of_type ty) ;
       ((x,ty)::ctx, (x, ref (Interpret.VClosure (env,e)))::env)
    | Syntax.Quit -> raise End_of_file

end) ;;

MiniHaskell.main () ;;

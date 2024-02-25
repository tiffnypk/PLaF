(**
   Tiffany Pak
   I pledge my honor that I have abided by the Stevens Honor System.
**)

open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds
    
(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> exp_val ea_result =
  fun e ->
  match e with
  | Int(n) ->
    return (NumVal n)
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1+n2))
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1-n2))
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1*n2))
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return (NumVal (n1/n2))
  | Let(id,def,body) ->
    eval_expr def >>= 
    extend_env id >>+
    eval_expr body 
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return (BoolVal (n = 0))
  | Pair(e1,e2) ->
    eval_expr e1 >>= fun ev1 ->
    eval_expr e2 >>= fun ev2 ->
    return (PairVal(ev1,ev2))
  | Fst(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun (l,_) ->
    return l
  | Snd(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun (_,r) ->
    return r
  | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str; 
    error "Debug called"

  | IsEmpty(e) -> 
    eval_expr e >>=
    tree_of_treeVal >>= fun t ->
    if t = Empty
    then return (BoolVal true)
    else return (BoolVal false)

  | EmptyTree(_t) -> 
    return (TreeVal(Empty))

  | Node (e1, e2, e3) -> 
    eval_expr e1 >>= fun v ->
    eval_expr e2 >>=
    tree_of_treeVal >>= fun left ->
    eval_expr e3 >>=
    tree_of_treeVal >>= fun right ->
    return (TreeVal (Node(v, left, right)))

  | CaseT(e1, e2, id1, id2, id3, e3) ->
    eval_expr e1 >>=
    tree_of_treeVal >>= fun t ->
    (match t with 
    | Empty -> eval_expr e2
    | Node(v, left, right) -> 
      extend_env id1 v >>+ extend_env id2 (TreeVal left) >>+ extend_env id3 (TreeVal right) >>+
      eval_expr e3)

  | Record(fs) ->
    let (x,y) = (List.split fs) in
    if (duplicates x)
    then error "Record: duplicate fields"
    else
      let (_,z) = (List.split y) in
      sequence(List.map eval_expr(z)) >>= fun rest -> return @@ RecordVal (List.combine x rest)

  | Proj(e, id) ->
    eval_expr e >>= fields_of_recordVal >>= fun l ->
    let rec find =
    fun x ->
    match x with 
    | [] -> error "Proj: field does not exist"
    | (y,z)::t -> 
    if y = id 
    then return (z)
    else find t
    in find l

  | _ -> failwith "Not implemented yet!"

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e


(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c
  








exception Unknown_Class of string
exception InvalidExpression of string
exception Typemismatch of string
exception Notyp_arg
exception Invalid_arg



let rec getListLast (l:string list) : (string list)*string=
	match l with
	| [] -> ([],"")
	| last::[] -> ([],last)
	| head::tail -> let (h,t) = getListLast tail in (head::h,t)

let raise_type_mismatch c x y =
  if c <> Some(Type.Primitive(Type.Boolean)) then raise(InvalidExpression("Le test doit être de type booleen"));
  match x, y with
  | Some(Type.Primitive(_)), None -> raise(Typemismatch("type mismatch"))
  | None, Some(Type.Primitive(_)) -> raise(Typemismatch("type mismatch"))
  | Some(typ1), Some(typ2) -> if typ1 <> typ2 then raise(Typemismatch("type mismatch"))

let compare_call arg b =
  match arg.etype with
  | None -> raise(Notyp_arg)
  | Some(t) -> if t <> b.ptype then raise(Invalid_arg)

let compare_arguments name args info = if List.length args <> List.length info.fargs then ()
  else try 
      List.iter2 compare_call args info.fargs;
      raise(InvalidExpression("Le test doit être de type booleen")))
	with | Invalid_arg -> ()

let rec check_array_type exp =
  match exp with
  | [] -> ()
  | head::tail -> (match t with
    | [] -> ()
    | h2::t2 -> if head.etype <> h2.etype then raise(InvalidExpression("Le type du tableau est invalide")));
 check_array_type tail (* ici check_array_type au lieu de check_array_list_type*)

let rec checkTypeExpression env_global env e =
  match e.edesc with
  | New(None, l, exp) -> List.iter (checkTypeExpression env_global env) exps;
    let (nl, last) = getListLast l in
    if (Hashtbl.mem env_global.classes last) <> true
    then raise(Unknown_class("la classe n'existe pas"))
    else
      (e.etype <- let constructeurs = (Hashtbl.find env_global.classes last).constructeurs in
      if (Hashtbl.length constructeurs == 0 && List.length exp == 0) then Some(Type.Ref({ tpath = []; tid = last }))
          List.iter (compare_arguments last exp) (Hashtbl.find_all constructeurs last);
          raise(Unknown_constructor("Constructeur Inconnu"))
       
  | NewArray(t, l, exp)-> 
			(match exp with
				|None -> e.etype <- Some(Type.Array(t, List.length l))
				|Some ep-> e.etype <- Some(Type.Array(t, List.length l)) 
				)
  | Call(exp, str, l)
			(match exp with
				|None ->List.iter (checkTypeExpression env_global env) l;
				|Some ep->List.iter (checkTypeExpression env_global env) l; checkTypeExpression env_global env ep;
					 (match ep with
    						 | { edesc = Name(id) } -> let cname = Type.stringOfOpt ep.etype in 
						    e.etype <- type_call_expr str l env_global cname)



  
  | Attr(exp, str) -> checkTypeExpression env_global env exp;
    (match exp with
     | { edesc = Name(id) } ->
     let cname = Type.stringOfOpt exp.etype in
     let att = (Hashtbl.find env_global.classes cname).attributs in
     e.etype <- (if Hashtbl.mem attrs str <> false
       then Some(Hashtbl.find att str)
       else raise(InvalidExpression("Cette variable n'est pas connue"))))

  | If(exp1, exp2, exp3) -> checkTypeExpression env_global env exp1; checkTypeExpression env_global env exp2; checkTypeExpression env_global env exp3

  | Val v -> e.etype <- (match v with
					| String s -> (Type.Ref { tpath = [] ; tid = "String" })
					| Int i -> (Type.Primitive Int)
					| Float f -> (Type.Primitive Float)
					| Char c -> (Type.Primitive Char)
					| Null ->  None
					| Boolean b -> (Type.Primitive Boolean)
)

  | Name(name) -> e.etype <- if (Hashtbl.mem env.vars_type name) <> true
    then (if (Hashtbl.mem (Hashtbl.find env_global.classes env_global.current).attributs name) <> true
      then raise(InvalidExpression("Cette variable n'est pas connue"))
      else Some(Hashtbl.find (Hashtbl.find env_global.classes env_global.current).attributs name))
    else Some(Hashtbl.find env.vars_type name)
  | ArrayInit(exp) -> List.iter (checkTypeExpression env_global env) exp;
    check_array_type exp;
    e.etype <- (match (List.hd exp).etype with
      | Some(t) -> Some(Type.Array(t, 1)))
 
  | AssignExp(e1, op, e2) -> checkTypeExpression env_global env e1; checkTypeExpression env_global env e2;
    if(e1.etype=e2.etype)
	then e1.etype
    e.etype <- e1.etype
  | Post(exp, op) -> checkTypeExpression env_global env exp;
	let t=exp.etype    
	match t with
				| Type.Primitive t1 -> (
					match t with
					| Boolean -> raise (InvalidExpression ("Pas adapté aux booleens"))
					| _ -> t1
				)
				| _ -> raise (InvalidExpression (""))
  | Pre(op, exp) -> checkTypeExpression env_global env exp;
  	match op with
  	| Op_not -> if exp.etype <> Some(Type.Primitive(Type.Boolean)) then raise(InvalidExpression ("Le type de l'expression n'est pas adéquat avec cette opération"))
  	| Op_bnot -> if exp.etype <> Some(Type.Primitive(Type.Int)) then raise(InvalidExpression ("Le type de l'expression n'est pas adéquat avec cette opération"))
  	| Op_neg | Op_incr | Op_decr | Op_plus -> if ( exp.etype<> Some(Type.Primitive(Type.Int)) && x <> Some(Type.Primitive		(Type.Float))) then raise(InvalidExpression ("Le type de l'expression n'est pas adéquat avec cette opération"))

    e.etype <- exp.etype
  
  | Op(e1, op, e2) -> checkTypeExpression env_global env e1; checkTypeExpression env_global env e2;
    (match op with
    | Op_cor | Op_cand
    | Op_eq | Op_ne | Op_gt | Op_lt | Op_ge | Op_le -> e.etype <- Some(Type.Primitive(Type.Boolean))

    | Op_or | Op_and | Op_xor
    | Op_shl | Op_shr | Op_shrr
    | Op_add | Op_sub | Op_mul | Op_div | Op_mod -> e.etype <- e1.etype)

  | CondOp(e1, e2, e3) -> checkTypeExpression env_global env e1; checkTypeExpression env_global env e2; checkTypeExpression env_global env e3;
    raise_type_mismatch e1.etype e2.etype e3.etype;
    if e2.etype <> None then e.etype <- e2.etype else e.etype <- e3.etype
  | Cast(t, exp) -> checkTypeExpression env_global env exp; e.etype <- Some(t)

  | Type t -> e.etype <- Some(t)

  | ClassOf t -> e.etype <- Some(t)

  | Instanceof(e1, t) -> checkTypeExpression env_global env e1; e.etype <- Some(Type.Primitive(Type.Boolean))
  | VoidClass -> ()





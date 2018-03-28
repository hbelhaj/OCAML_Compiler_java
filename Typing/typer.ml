open AST
open Location
open Type

type function_desc = {
  ftype: Type.t;
  fargs: argument list
}

type env_classe = {
  classe_parent : Type.ref_type;
  constructeurs : (string, function_desc) Hashtbl.t;
   methodes : (string, function_desc) Hashtbl.t;
   attributs : (string, Type.t) Hashtbl.t;
  
}
type env_global = {
  classes : (string, env_classe) Hashtbl.t ;
  mutable current : string
}







let vType v =
  match v with
  | Boolean b -> Some(Type.Primitive(Type.Boolean)) 
  | Char c -> Some(Type.Primitive(Type.Char))
  | Int i -> Some(Type.Primitive(Type.Int))
  | Float f -> Some(Type.Primitive(Type.Float))
  | Null -> None
  | String s -> Some(Type.Ref({ tpath = []; tid = "String" })) 

type env = {
  return_type: Type.t;
  vars: (string, Type.t) Hashtbl.t
}


let env_classes_static = 
	    [
                {name="Boolean"; classe_parent=Some "Object"; methodes=[]; attributs=[]};
		{name="Int"; classe_parent=Some "Object"; methodes=[]; attributs=[]};
		{name="Object"; classe_parent=None; methodes=[]; attributes=[]};
                {name="String"; classe_parent=Some "Object"; methodes=[]; attributs=[]};
            ]

 (* typing a class *)
let rec type_of_classname current classename loc = 
	match current with 
	| [] -> raise (PError(UndefinedType(string_of_classname cn), loc)) 
	| t::q when t.name = (string_of_classname classename) -> t.name
        | t::q -> type_of_classname q classename loc


(* return class definition *)

let rec get_classdef env_classe classname loc = 
	let func c = 
		if (c.cname = classname) then true else false
	in match env_classe with
	(*| [] -> raise (PError(UndefinedType(classname), loc))  *)
        | t::q -> if (func t) then t else get_classdef q classname loc

(* deprecated *) (* to remove *)

let rec is_parent env_classe classe_parent classe_fille =
	match classe_parent, classe_fille with 
	| None, _ | _, None -> false
	| _, Some classename -> 
		let classdef_fille = get_classdef env_classe classename Location.none
		in if (classdef_fille.classe_parent = classe_parent) then true else 
			
   is_parent env_classe classe_parent classdef_fille.classe_parent

(* check reference type given the global env and real type and apparent type *)
let rec verif_ref_type ( gen_env : env_global) (type_apparent : Type.ref_type) (type_reel : Type.ref_type) =

if type_apparent.tid <> type_reel.tid then
 if type_apparent.tid = "Object" then () else
	let pere = (Hashtbl.find gen_env.classes type_reel.tid).classe_parent in
	 if type_apparent <> pere then verif_ref_type gen_env type_apparent pere
	 (* else pere.tid = "Object" then raise( ) *)


(* add a variable to env *)

let ajouter_variable env name vartype =
 if Hashtbl.mem env.vars name<> true 
	then Hashtbl.add env.vars name vartype
(*else raise exception *)



(*type variable declaration and add it to env *)


let type_declar_var gen_env env decl =
 match decl with
	|(t,name,None) -> ajouter_variable env name t
	|(t, name, Some e) -> checkTypeExpression gen_env env e;
	(* if Some(t) <> e.etype then raise exception *) 
	|(None , name , None) -> ()
	|(None , name , Some e) -> ()
	





(* add args of method or a constructor to env *)

let ajouter_fonction_args env func = if (Hashtbl.mem env.vars func.pident) <> true 
then Hashtbl.add env.vars func.pident func.ptype

(* else raise exception *)

(* type a method and constructor and a create a new env for it *)


let type_method gen_env meth = 
let env = { return_type = meth.return_type ; vars = Hashtbl.create 20 } in

 List.iter (ajouter_fonction_args env ) meth.margstype; 
List.iter (type_statement gen_env env) meth.mbody




let type_constructeur gen_env construct =
let cenv = { return_type = Type.Ref ({tpath = [] ; tid = construct.cname }); vars = Hashtbl.create 20} in 

List.iter (ajouter_fonction_args cenv ) construct.cargstype;
List.iter (type_statement gen_env cenv) construct.cbody


(* type an attribute *)


let type_attribut gen_env att = match att.adefault with
	|None -> ()
	|Some(e) -> checkTypeExpression gen_env { returntype = Type.Void; vars =Hashtbl.create 1} e; let type_attribut = Some(att.atype) in verif_ref_type gen_env type_attribut e.etype

(*check if  *)
let verif_assign_operator_type x op y =
  if x <> y then
    match x with
     | Some(Type.Ref(type_apparent)) -> if y <> None then
       (match y with
        | Some(Type.Ref(type_reel)) -> verif_ref_type gen_env type_apparent type_reel )
      (*  | _ -> raise *)
(* | _ -> raise *)


(*type class *)

let type_class gen_env cl = List.iter ( type_method gen_env) cl.cmethods; List.iter ( type_constructeur gen_env) cl.cconsts; List.iter ( type_attribut gen_env) cl.cattributes

(* compare arguments of a constructor given a list of arguments *)

let args_fonction_compare name args args2 =
if List.length args <> List.length args2.fargs then ()
	(*else try 
	List.iter2 arg_compare args args2.fargs;
	 raise exception if all arguments are the same 
	with 
	| exception  *)


 let arg_compare x y = ()(*if  x.ptype <> y.ptype then raise(exception) *)	


(* add method , constructor , attribut *)

let ajouter_methode gen_env met =
	let m = (Hashtbl.find gen_env.classes gen_env.current).methods in 
		(if (Hashtbl.mem m  met.mname) <> true
			then Hashtbl.add m met.mname {ftype = met.mreturntype ; fargs = met.margstype}
(* find all methods with the same name, compare args and see if there is an exception *)		
		else
		(List.iter ( args_fonction_compare met.mname met.margstype)(Hashtbl.find_all m met.mname);
		Hashtbl.add m met.mname { ftype = met.mreturntype ; fargs = met.margstype }))
		


let ajouter_attribut gen_env att = 
	if (Hashtbl.mem (Hashtbl.find gen_env.classes gen_env.current).attributes att.aname) <> true
 	then Hashtbl.add (Hashtbl.find gen_env.classes gen_env.current).attributes att.aname att.atype
	(* else raise *)

let check_return_type x y =
  match x, y with
  | _, None -> raise(InvalidReturn("No Return type ")
  | x, Some(z) -> if x <> z then raise(InvalidReturn("Return type not appropriated")


let ajouter_constructeur gen_env constr = 
   let c = (Hashtbl.find gen_env.classes gen_env.current).constructors in 
	(if (Hashtbl.mem c constr.cname) <> true
		then Hashtbl.add c constr.cname {ftype=Type.Ref ({tpath=[] ; tid = constr.cname}) ; fargs = constr.cargstype }
	else
	(List.iter ( args_fonction_compare constr.cname constr.cargstype) (Hashtbl.find_all c constr.cname);
	Hashtbl.add c constr.cname {ftype=Type.Ref ({tpath=[] ; tid = constr.cname}) ; fargs = constr.cargstype }))



(* add a class to gen_env *)

let ajouter_class gen_env cl = 
	List.iter(ajouter_methode gen env) cl.cmethods;
	List.iter(ajouter_attribut gen_env) cl.cattributes;
	List.iter(ajouter_constructeur gen_env) cl.cconsts



(* create the env for a class *)




let ajouter_env_classe  gen_env t = match t.info with
	
	|Inter -> ()

	|Class c -> if (Hashtbl.mem gen_env.classes t.id) <> true
		then (gen_env.current <- t.id ; Hashtbl.add gen_env.classes gen_env.current {attributs = (Hashtbl.create 20); methodes = (Hashtbl.create 20); constructeurs = (Hashtbl.create 20); classe_parent = c.cparent})
	(* else raise *)
		ajouter_classe gen_env c
	
(* type a program , construct gen_env and type every class and interface *)

let type_classe_inter gen_env t = match t.info with 
	|Inter -> ()
	|Class c -> gen_env.current <-t.id; type_class gen_env c

let type_program p =    let p_env = { classes = Hashtbl.create 20; current =""} in
					List.iter(ajouter_env_classe p_env) p.type_list;
					List.iter(type_classe_inter p_env)p.type_list





let rec type_statement gen_env env statement =
  match statement with
  | VarDecl(l) -> List.iter (type_declar_var gen_env env ) l
  | Block b -> let newscope = { return_type =env_global.return_type; vars = Hashtbl.copy env.vars } in
    List.iter (type_statement gen_env newscope) b
  | Nop -> ()
  | While(e, s) -> checkTypeExpression gen_env env e; type_statement gen_env env s
  | For(l, None, exps, s) -> let forScope = { return_type = env.return_type; vars = Hashtbl.copy env.vars } in
    List.iter (type_for_vardecl gen_env forScope) l;
    List.iter (checkTypeExpression gen_env forScope) exps; type_statement gen_env forScope s
  | For(l, Some(exp), exps, s) -> let forScope = { return_type = env.return_type; vars = Hashtbl.copy env.vars } in
    List.iter (type_for_vardecl gen_env forScope) l;
    checkTypeExpression gen_env forScope exp; 
    if exp.etype <> Some(Type.Primitive(Type.Boolean)) then raise(InvalidExpression("Le type doit être de type booleen"));
    List.iter (checkTypeExpression gen_env forScope) exps; type_statement gen_env forScope s
  | If(e, s, None) -> checkTypeExpression gen_env env e; type_statement gen_env env s;
     if e.etype <> Some(Type.Primitive(Type.Boolean)) then raise(InvalidExpression("Le type doit être de type booleen"))
  | If(e, s1, Some(s2)) -> checkTypeExpression gen_env env e; type_statement gen_env env s1; type_statement gen_env env s2;
     if e.etype <> Some(Type.Primitive(Type.Boolean)) then raise(InvalidExpression("Le type doit être de type booleen"))
  | Return None -> if env.return_type <> Type.Void then raise(InvalidReturn("Le type de retour n est pas conforme "))
  | Return Some(e) -> checkTypeExpression gen_env env e; check_return_type env.return_type e.etype
  | Throw e -> checkTypeExpression gen_env env e
  | Try(s1, l, s2) -> List.iter (type_statement gen_env env) s1; List.iter (type_statement gen_env env) s2;
    List.iter (type_catches gen_env env) l
  | Expr e -> checkTypeExpression gen_env env e




(* let rec type_for_vardecl  *)





(* to do type statement *)
(*to do add method and constructor and attributes to env*)  (***** done *****)
(* typing a program *) (***** done *****)
(* to do function for calling methods *)
(* to do errors and exceptions *)
(* testing *)

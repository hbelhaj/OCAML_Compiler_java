open AST 

type type_att= 
  | TInt of int
  | TBool of bool
  | TString of string
  | TRef of int				
  | TNull 
  | TNom of string 	
  | TAttr of string * string  		



type descripteur_objet =
{
    nom_obj : string;
    type_obj : string;
    attributs_obj : (string, type_att) Hashtbl.t;
}

type descripteur_classe =
{
	nom_classe : string;
        classe_parent : string;
	methodes_classe : (string, astmethod) Hashtbl.t;		
        constructeurs_classe : (string, astconst) Hashtbl.t;
        attributs_classe : (string,descripteur_objet) Hashtbl.t;
        
}

type descripteurs_obj =
  | ObjectDescriptor of descripteur_objet
  | IntegerDescriptor of int  
  | StringDescriptor of string 
  | NullObject 
 
  
  
type descripteurs_cl=
  | ClassDescriptor of descripteur_classe
  | ObjectClass of descripteur_classe (* allocation des classes particulières *)
  | StringClass
  | IntegerClass
  | BooleanClass

type tables = 
{
  table_methodes : (string, astmethod) Hashtbl.t;
  table_attclasses: (string, descripteurs_cl) Hashtbl.t  (* table du contenu de la classe (nom de la classe, son descripteur)*)
}
(* construire la table pour les attributs de la classe*)

let build_classe_table  clTable =
let desc_cl = { nom_classe = "Object";classe_parent=""; methodes = Hashtbl.create 20; constructeurs = Hashtbl.create 10; attributs = [] } in
Hashtbl.add clTable "Object" (ObjectClass(desc_cl));
Hashtbl.add clTable"String" StringClass;
Hashtbl.add clTable "Int" IntegerClass;
Hashtbl.add clTable "Boolean" BooleanClass


 
(* ajouter les méthodes de la classe courante et de la classe mere à la table des méthodes *)

 let ajouter_methodes table_methodes methodes_cl nom_classe methode =
   let m_desc= nom_classe ^ "_" ^ methode.mname in 
     Hashtbl.add methodes_cl methode.mname methode
     Hashtbl.add table_methodes m_desc methode

  let ajouter_meth_classemere =()


(* ajouter les constructeurs et les attributs de la classe à la table des méthodes *)
 


let ajouter_constr  =()

 


let ajouter_attributs  = ()


let ajouter_contenuClasse_table table_attclasses classe nom_classe table_methodes =

  let allmethodes = Hashtbl.create 20 in
      let desc_clparent = Hashtbl.find table_attclasses classe.cparent.tid in
      List.iter (ajouter_methodes table_methodes methodes  nom_classe) classe.cmethods;
	
  

(* si la classe nest pas compilee on compile sa classe mere *)
let is_compiled nom_classe table_attclasses = match nom_classe with
  | "Object" -> true
  | _ -> Hashtbl.mem table_attclasses nom_classe


let rec retourner_classeparent nom_classe c = match c with
| h::q -> if h.id = nom_classe then h else retourner_classeparent nom_classe q



(* fonction compile *)

let compile = 







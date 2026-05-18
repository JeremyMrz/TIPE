(* correspondant à un char *)
type gen = string

(* correspondant à une liste de strings *)
type elem = string

(* correpsond à l'ensemble des classes d'équivalences (partition des éléments) *)
type cl_equivalence = elem list list

(* structure de graphe *)
type 'a mat = 'a array array
type graphe = elem array * bool mat

exception Argument_Failure

(* initialise le graphe avec une matrice d'adjacence nulle *)
let init_graphe () : graphe =
  ( [| "1"; "r"; "a"; "aa" |],
    [|
      [| true; true; false; true |];
      [| false; true; false; false |];
      [| false; false; true; false |];
      [| false; false; false; true |];
    |] )

let gen = [| "a"; "r" |]
let axioms = [ [ "a"; "aaa"; "1a"; "a1" ]; [ "1" ]; [ "r"; "r1"; "r1" ] ]

(* signe des générateurs produit à gauche *)
let lsign (gen : gen) : int =
  match gen with "r" -> 1 | "a" -> -1 | _ -> raise Argument_Failure

(* signe des générateurs produit à droite *)
let rsign (gen : gen) : int =
  match gen with "r" -> 1 | "a" -> 1 | _ -> raise Argument_Failure

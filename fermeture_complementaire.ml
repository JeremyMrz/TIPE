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
  ( [| "1"; "k"; "c" |],
    [|
      [| true; true; false |];
      [| false; true; false |];
      [| false; false; true |];
    |] )

let gen = [| "c"; "k" |]
let axioms = [ [ "k"; "kk"; "1k"; "k1" ]; [ "1"; "cc" ] ]

(* signe des générateurs produit à gauche *)
let lsign (gen : gen) : int =
  match gen with "k" -> 1 | "c" -> -1 | _ -> raise Argument_Failure

(* signe des générateurs produit à droite *)
let rsign (gen : gen) : int =
  match gen with "k" -> 1 | "c" -> 1 | _ -> raise Argument_Failure

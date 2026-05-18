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
  ( [| "1"; "k"; "c"; "i" |],
    [|
      [| true; true; false; false |];
      [| false; true; false; false |];
      [| false; false; true; false |];
      [| true; true; false; true |];
    |] )

let gen = [| "c"; "k"; "i" |]

let axioms =
  [
    [ "k"; "kk"; "1k"; "k1" ];
    [ "1"; "cc" ];
    [ "i"; "1i"; "i1"; "ii"; "ckc" ];
    [ "c"; "1c"; "c1" ];
  ]

(* signe des générateurs produit à gauche *)
let lsign (gen : gen) : int =
  match gen with "k" -> 1 | "c" -> -1 | "i" -> 1 | _ -> raise Argument_Failure

(* signe des générateurs produit à droite *)
let rsign (gen : gen) : int =
  match gen with "k" -> 1 | "c" -> 1 | "i" -> 1 | _ -> raise Argument_Failure

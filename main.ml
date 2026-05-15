(* 
INITIALISATION :

- Donner une liste d'opérateurs et de règles
- Initialiser alors le graphe avec simplement le neutre

PROGRAMME :

- Multiplier le graphe précédent par les opérateurs de l'ensemble (à gauche et à droite) ; (Transpation)
- Combiner l'ensemble des graphes obtenus
- Chercher les strongly connected graphs grâce à l'algorithme de Tarjan
- Rajouter ces éléments égaux aux classes d'équivalence
- Appliquer l'algorithme de Knuth-Bendix avec l'ordre shortlex pour orienter -> détermine un représentant canonique
- Simplifier l'ancien graphe avec simplement les représentants canoniques
- Simplifier le nouveau graphe avec simplement les représentants canoniques
- Vérifier si le graphe nouveau graphe est égal au graphe précédent
- Itérer le Processus si ce n'est pas le cas (s'arrêter selon l'input de l'utilisateur)
...

- Imprimer le graphe obtenu (soit en prenant les représentants canoniques de Knuth-Bendix, ou en utilisant un shortlex)
- Terminer 
*)

(* correspondant à un char *)
type gen = string

(* correspondant à une liste de strings *)
type elem = string

(* correpsond à l'ensemble des classes d'équivalences (partition des éléments) *)
type cl_equivalence = elem list list

(* structure de graphe *)
type 'a mat = 'a array array
type graphe = elem array * bool mat

exception Liste_Vide
exception Argument_Failure

(* ----------------------------------------------------------------------- UTILES *)
(*------------------ PILE *)
(* empile x au dessus de la pile p *)
let empiler (p : 'a list) (x : 'a) : 'a list = x :: p

(* depile p = x::q, renvoie (x,q)
remarque : renvoie une erreur "List_Vide" si p est vide *)
let depiler (p : 'a list) : 'a * 'a list =
  match p with [] -> raise Liste_Vide | x :: q -> (x, q)

(* renvoie true si x est dans p, sinon false *)
let rec recherche (p : 'a list) (x : 'a) : bool = List.mem x p

(*------------------ STRING *)
(* renvoie true si s1 est plus petit que s2 selon l'ordre shortlex *)
let shortlex (s1 : string) (s2 : string) : bool =
  let n1 = String.length s1 in
  let n2 = String.length s2 in
  (* renvoie le plus petit élément de ss1 et de ss2 selon l'ordre lex *)
  let lex (ss1 : string) (ss2 : string) : bool =
    if ss1 <= ss2 then true else false
  in
  if n1 < n2 then true else if n2 < n1 then false else lex s1 s2

(*------------------ SORT *)
(* tri fusion ordre croissant selon l'ordre shortlex *)
let rec fusion_sort (l : 'a list) (f : 'a -> 'a -> bool) : 'a list =
  let rec split (l : 'a list) : 'a list * 'a list =
    match l with
    | [] | _ :: [] -> (l, [])
    | x1 :: x2 :: q ->
        let l1, l2 = split q in
        (x1 :: l1, x2 :: l2)
  in
  let rec fuse (l1 : 'a list) (l2 : 'a list) =
    match (l1, l2) with
    | [], l | l, [] -> l
    | x :: l1', y :: l2' -> if f x y then x :: fuse l1' l2 else y :: fuse l1 l2'
  in
  match l with
  | [] | _ :: [] -> l
  | _ ->
      let l1, l2 = split l in
      fuse (fusion_sort l1 f) (fusion_sort l2 f)

(* renvoie l'union des termes de l1 et l2 *)
let union (l1 : 'a list) (l2 : 'a list) : 'a list =
  List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) l1 l2

(* renvoie l'indice de x dans l *)
let index_of (x : 'a) (l : 'a list) : int =
  match List.find_index (fun y -> y = x) l with
  | Some i -> i
  | None -> raise Liste_Vide

(* ----------------------------------------------------------------------- GENERATEURS *)

(* signe des générateurs produit à gauche *)
let lsign (gen : gen) : int =
  match gen with "k" -> 1 | "c" -> -1 | _ -> raise Argument_Failure

(* signe des générateurs produit à droite *)
let rsign (gen : gen) : int =
  match gen with "k" -> 1 | "c" -> 1 | _ -> raise Argument_Failure

(* fusionne deux classes d'équivalences 
remarque : préserve l'ensemble des éléments des deux classes d'éuivalence *)
let fuse_cl_eq (cl1 : cl_equivalence) (cl2 : cl_equivalence) : cl_equivalence =
  (* renvoie la fusion d'un ensemble de classes d'équivalences avec une simple classe d'équivalence *)
  let fuse_elem (cl : cl_equivalence) (l : elem list) : cl_equivalence =
    let cl', l' =
      List.partition
        (fun classe -> List.exists (fun x -> List.mem x l) classe)
        cl
    in
    List.fold_left union l l' :: cl'
  in
  List.fold_left fuse_elem cl1 cl2

(* renvoie le représentant canonique selon l'ordre shortlex d'une classe d'equivalence *)
let shortlex_classe (cl : cl_equivalence) : elem list =
  let canonique (l : elem list) : elem =
    match fusion_sort l shortlex with [] -> raise Liste_Vide | x :: _ -> x
  in
  List.map canonique cl

(* renvoie le représentant canonique d'un élément x dans cl en fonction de cl_canonique (que l'on suppose dans le même ordre)
remarque : x dans se trouver dans une classe d'équivalence *)
let rec canonique_of (x : elem) (cl : cl_equivalence) (cl_canonique : elem list)
    : elem =
  match cl with
  | [] -> raise Liste_Vide
  | l :: q ->
      if List.mem x l then List.hd cl_canonique
      else canonique_of x q (List.tl cl_canonique)

(* ----------------------------------------------------------------------- GRAPHE *)

let init_matrix n m f = Array.init n (fun i -> Array.init m (fun j -> f i j))
let get_ a i j = Array.get (Array.get a i) j

let transpose a n m =
  let tr_ a i j = Array.get (Array.get a j) i in
  init_matrix m n (tr_ a)

(* initialise le graphe avec une matrice d'adjacence nulle *)
let init_graphe (l : elem array) : graphe =
  let n = Array.length l in
  (l, init_matrix n n (fun _ _ -> false))

(* mutliplie un graphe à gauche *)
let l_mult_graphe ((s, m) : graphe) (gen : gen) : graphe =
  let s' = Array.map (fun x -> gen ^ x) s in
  let n = Array.length s in
  let m' = if lsign gen = 1 then m else transpose m n n in
  (s', m')

(* mutliplie un graphe à droite *)
let r_mult_graphe ((s, m) : graphe) (gen : gen) : graphe =
  let s' = Array.map (fun x -> x ^ gen) s in
  let n = Array.length s in
  let m' = if rsign gen = 1 then m else transpose m n n in
  (s', m')

(* fusionne une liste de graphes 
remarque : préserve l'ensemble des sommets/arrêtes et chaque graphe doit être de même taille *)
let fuse_graphe (l : graphe array) : graphe =
  let n = Array.length (fst l.(0)) in
  (* renvoie l'union de a1, a2 sans doublons *)
  let union_array (a1 : 'a array) (a2 : 'a array) : 'a array =
    let l1 = Array.to_list a1 in
    let l2 = Array.to_list a2 in
    Array.of_list (union l1 l2)
  in
  let s' = Array.fold_left (fun x (s, _) -> union_array x s) [||] l in
  let n' = Array.length s' in
  let m' = init_matrix n' n' (fun _ _ -> false) in

  (* fusionne les sommets directement de manière impérative *)
  Array.iter
    (fun (s, m) ->
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          let i' = index_of s.(i) (Array.to_list s') in
          let j' = index_of s.(j) (Array.to_list s') in
          m'.(i').(j') <- m'.(i').(j') || m.(i).(j)
        done
      done)
    l;
  (s', m')

(* renvoie true si g1 = g2 en terme de sommets, false sinon
remarques : les sommets doivent être tous distincts *)
let compare_graphe ((s1, m1) : graphe) ((s2, m2) : graphe) : bool =
  Array.length s1 = Array.length s2
  && Array.for_all (fun x -> Array.exists (fun y -> y = x) s2) s1

(* renvoie le graphe consistué des éléments canoniques cl *)
let canonique_graphe ((s, m) : graphe) (cl : cl_equivalence) : graphe =
  let n = Array.length s in
  (* nouveau graphe *)
  let cl' = shortlex_classe cl in
  let s' = Array.of_list cl' in
  let n' = Array.length s' in
  let m' = init_matrix n' n' (fun _ _ -> false) in

  for i = 0 to n - 1 do
    begin
      for j = 0 to n - 1 do
        begin
          (* renvoie les indices des réprésentants canoniques associées dans la nouvelle matrice *)
          let i' = index_of (canonique_of s.(i) cl cl') cl' in
          let j' = index_of (canonique_of s.(j) cl cl') cl' in
          m'.(i').(j') <- m'.(i').(j') || m.(i).(j)
        end
      done
    end
  done;
  (s', m')

(* imprime le graphe *)
let print_graphe (g : graphe) : unit = ()

(* ----------------------------------------------------------------------- TARJAN *)
(* renvoie les composantes fortements connectées de g *)
let tarjan ((sommets, mat) : graphe) : string list list =
  let n = Array.length sommets in

  (* initialisation des composantes *)
  let indice = Array.make n (-1) in
  let lien_min = Array.make n 0 in
  let indice_pile = Array.make n false in

  (* noeuds visites sans composantes fortes attribuées *)
  let pile = ref [] in

  (* nombre de noeuds visites *)
  let c = ref 0 in
  let res = ref [] in

  let rec parcours_profondeur s =
    (* noveau noeud *)
    indice.(s) <- !c;
    lien_min.(s) <- !c;
    c := !c + 1;

    (* s pris en compte dans les composantes non fortement connectées*)
    pile := empiler !pile s;
    indice_pile.(s) <- true;

    for voisin = 0 to n - 1 do
      if get_ mat s voisin then begin
        (* non visité *)
        if indice.(voisin) = -1 then begin
          parcours_profondeur voisin;
          lien_min.(s) <- min lien_min.(s) lien_min.(voisin)
        end (* visité et dans la pile *)
        else if indice_pile.(voisin) then
          lien_min.(s) <- min lien_min.(s) indice.(voisin)
      end (* visité mais pas dans la pile *)
    done;

    (* vérification de la présence d'une composante fortement connectée *)
    if lien_min.(s) = indice.(s) then begin
      let composante = ref [] in
      let continuer = ref true in
      while !continuer do
        let x, q = depiler !pile in
        pile := q;
        composante := sommets.(x) :: !composante;
        indice_pile.(x) <- false;
        (* On s'arrête quand on a dépilé v lui-même *)
        if x = s then continuer := false
      done;
      (* On ajoute la composante finalisée à la liste des résultats *)
      res := !composante :: !res
    end
  in

  (* parcours en profondeur sur chaque noeud non visité *)
  for s = 0 to n - 1 do
    if indice.(s) = -1 then parcours_profondeur s
  done;

  (* On retourne la liste de toutes les composantes trouvées *)
  !res

(* ----------------------------------------------------------------------- KNUTH-BENDIX  *)

(* renvoie les nouvelles classes d'équivalences à partir des précédentes selon knuth bendix *)
let knuth_bendix (cl : cl_equivalence) : cl_equivalence = cl

(* ----------------------------------------------------------------------- TESTS *)
let tests () = ()

(* ----------------------------------------------------------------------- MAIN *)
(* renvoie l'approximation d'ordre n du graphe *)
let approx_n (n : int) (gen : string array) (axioms : string list list) : graphe
    =
  let g = ref (init_graphe [| "" |]) in
  let n_gen = Array.length gen in
  let cl_eq = ref axioms in

  let approx_next (gg : graphe) : graphe =
    let new_graphe = Array.make (2 * n_gen) ([||], [| [||] |]) in
    for i = 0 to n_gen - 1 do
      begin
        new_graphe.(2 * i) <- l_mult_graphe gg gen.(i);
        new_graphe.((2 * i) + 1) <- r_mult_graphe gg gen.(i)
      end
    done;
    let g' = fuse_graphe new_graphe in
    let cl_eq' = tarjan g' in
    cl_eq := fuse_cl_eq !cl_eq cl_eq';
    cl_eq := knuth_bendix !cl_eq;
    canonique_graphe g' !cl_eq
  in
  let i = ref 0 in
  let continuer = ref true in
  while !continuer && !i < n do
    let prev = !g in
    g := approx_next !g;
    i := !i + 1;
    if compare_graphe !g prev then continuer := false
  done;
  !g

let main () =
  try
    if Array.length Sys.argv <= 2 then raise Argument_Failure;
    if Sys.argv.(1) = "test" then (
      print_string "Vérification des tests... \n";
      tests ())
    else
      let n = int_of_string Sys.argv.(1) in
      let gen = [| "k"; "c" |] in
      let axioms = [ [ "k"; "kk" ]; [ ""; "cc" ] ] in
      let g = approx_n n gen axioms in
      print_graphe g
  with Argument_Failure ->
    print_string "Argument Failure : mauvais arguments \n\n"

let _ = main ()

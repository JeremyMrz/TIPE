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

type 'a mat = 'a array array
type graphe = string array * bool mat

exception Liste_Vide
exception Argument_Failure

let init_matrix n m f = Array.init n (fun i -> Array.init m (fun j -> f i j))

let get_   a i j = Array.get (Array.get a i) j
let tr_   a i j = Array.get (Array.get a j) i
let transpose  a n m = init_matrix m n (tr_ a)


let l_graph_mult (g:gen) (n,l,m:graphe) : graphe =
  let n = Array.lenght(s) in
  let s' = Array.map (mult [|g|]) l
in
  let m' = if lsign g = 1 then m else transpose m n n
in
  (n,l',m')

(* ----------------------------------------------------------------------- GENERATEURS *)
let lsign (g: string) : int =
 match g with
  |"k" -> 1
  |"c" -> (-1)
  | _ -> 

let rsign (g:gen) : int =
 match g with
  |K -> 1
  |C -> 1


(* ----------------------------------------------------------------------- GRAPHE *)
(* initialise le graphe *)
let init_graphe () : graphe = ([||], [| [||] |])
let mult_graphe (g : graphe) (gen : string) : graphe = ([||], [| [||] |])
let fuse_graphe (l : graphe array) : graphe = ([||], [| [||] |])


(* renvoie true si s1 est plus petit que s2 selon l'ordre shortlex *)
let shortlex (s1 : string) (s2 : string) : bool =
  let n1 = String.length s1 in
  let n2 = String.length s2 in
  (* renvoie le plus petit élément de ss1 et de ss2 selon l'ordre lex *)
  let lex (ss1 : string) (ss2 : string) : bool =
    if ss1 <= ss2 then true else false
  in
  if n1 < n2 then true else if n2 < n1 then false else lex s1 s2

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

(* ----------------------------------------------------------------------- PILE *)
(* empile x au dessus de la pile p *)
let empiler (p : 'a list) (x : 'a) : 'a list = x :: p

(* depile p = x::q, renvoie (x,q)
remarque : renvoie une erreur "List_Vide" si p est vide *)
let depiler (p : 'a list) : 'a * 'a list =
  match p with [] -> raise Liste_Vide | x :: q -> (x, q)

(* renvoie true si x est dans p, sinon false *)
let rec recherche (p : 'a list) (x : 'a) : bool = List.mem x p

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
      if mat.(s).(voisin) = 1 then begin
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

(* renvoie le représentant canonique selon l'ordre shortlex d'une classe d'equivalence supposée d'élément triée
remarque : la liste ne doit pas être vide *)
let canonique (l : string list) : string =
  match l with [] -> raise Liste_Vide | x :: q -> x

let rec approx_n (n : int) (gen : string array) : graphe =
  let g = ref (init_graphe ()) in
  let n_gen = Array.length gen in
  let approx_next (gg : graphe) : graphe =
    let new_graphe = Array.make (2 * n_gen) ([||], [| [||] |]) in
    for i = 0 to n_gen do
      begin
        new_graphe.(2 * i) <- l_mult_graphe !g gen.(i);
        new_graphe.((2 * i) + 1) <- r_mult_graphe !g gen.(i)
      end
    done;
    fuse_graphe new_graphe
  in
  for (i = 0) to n do 
    begin
      g != approx_next(!g)
    end
  done;
  ([||], [| [||] |])

let print_graphe (g : graphe) : unit = ()

(* ----------------------------------------------------------------------- TESTS *)
let tests () = ()

let main () =
  try
    if Array.length Sys.argv <= 2 then raise Argument_Failure;
    if Sys.argv.(1) = "test" then (
      print_string "Vérification des tests... \n";
      tests ())
    else
      let n = int_of_string Sys.argv.(1) in
      let gen = [| "k"; "c" |] in
      let g = approx_n n gen in
      print_graphe g
  with Argument_Failure ->
    print_string "Argument Failure : mauvais arguments \n\n"

let _ = main ()


(*un petit paff en plus*)
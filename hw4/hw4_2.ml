(*
   SNU 4190.310 Programming Languages (Fall 2014)
   Gwangrae Kim, k.gwangrae@gmail.com
   Treasure Island / Type inference
*)
 
type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key
type map = End of treasure
         | Branch of map * map
         | Guide of string * map

let rec removeDup : key list -> key list = fun l -> 
  match l with 
  | h::t -> if (List.mem h t) then (removeDup t) else h::(removeDup t)
  | [] -> []

exception IMPOSSIBLE

type shape = Free
            | FreeBox of string (*Mutable type, but binded to NameBox*)
            | Key of key (*No wildcard inside*)
            | Division of shape * shape

let nameToShapeTbl = Hashtbl.create 50
let equalityPairsTbl = Hashtbl.create 50

let getShape : treasure -> shape = fun t ->
  Hashtbl.find nameToShapeTbl t

let rec containSelfEquality : string -> shape -> bool = fun name s_new ->
  match s_new with
  | FreeBox (str) -> (name=str) || 
                   (List.mem name (Hashtbl.find_all equalityPairsTbl str))
  | Division (l, r) -> (containSelfEquality name l) || (containSelfEquality name r)
  | _ -> false

let rec calcShape : shape -> shape -> shape = fun s_orig s_new ->
  match s_orig, s_new with
    | Free, _ -> s_new (* 'Free' type can mutate into anything *)
    | FreeBox (a), FreeBox (b) -> let _ = Hashtbl.add equalityPairsTbl a b in
                                  let _ = Hashtbl.add equalityPairsTbl b a in s_new
    | FreeBox (a), Free -> s_orig
    | FreeBox (str), _ -> if (containSelfEquality str s_new) then raise IMPOSSIBLE else s_new
    | _, Free -> s_orig
    | _, FreeBox (str) -> if (containSelfEquality str s_orig) then raise IMPOSSIBLE else s_orig 
    | Division (l_orig, r_orig), Division (l_new, r_new) -> 
        Division ((calcShape l_orig l_new), (calcShape r_orig r_new))
    | Division (l_orig, r_orig), Key (Node (l_new, r_new)) -> 
        Division ((calcShape l_orig (Key(l_new))),(calcShape r_orig (Key(r_new))))
    | Key (Node (l_orig, r_orig)), Division (l_new, r_new) -> 
        Division ((calcShape (Key(l_orig)) l_new),(calcShape (Key(r_orig)) r_new))
    | Key (k_orig), Key (k_new) -> if (k_orig=k_new) then s_new else raise IMPOSSIBLE 
    | _, _ -> raise IMPOSSIBLE

let bindName : treasure -> shape -> shape = fun t s ->
  if (Hashtbl.mem nameToShapeTbl t)
  then (
    let s_new = (calcShape (getShape t) s) in
    let _ = (Hashtbl.replace nameToShapeTbl t s_new) in
    s_new
  )
  else (let s_new = (calcShape (match t with 
                                | StarBox -> Key (Bar)
                                | NameBox (str) -> FreeBox(str)
                               ) s) 
        in
        let _ = Hashtbl.add nameToShapeTbl t s_new in
        s_new)
  
let rec shapeToKey : shape -> key = fun s ->
  match s with 
  | Free -> Bar
  | FreeBox (_) -> Bar (*Not really sure, but may work if there were no contradiction *)
  | Key k -> k
  | Division (l,r) -> Node ((shapeToKey l),(shapeToKey r))

(* m : map, 
 * impl : what shape this map should imply, determined by parent node of the map
 * This function verifies whether the designated shape is obtainable *)
let rec eval : map -> shape -> shape =
  fun m impl -> 
  match m with
  | End t -> (bindName t impl)
  | Guide (str,m_in) -> 
      let solve : shape -> shape -> shape = fun s1 s2 -> 
        (Division ((bindName (NameBox str) s1), (eval m_in s2))) 
      in
      (match impl with
       | Free -> (solve Free Free)
       | FreeBox (str) -> (solve (FreeBox str) (FreeBox str)) (*Not sure*)
       | Division (s1, s2) -> (solve s1 s2)
       | Key (Node(l,r)) -> (solve (Key l) (Key r))
       | _ -> raise IMPOSSIBLE  
      )
  | Branch (m1, m2) -> (
    match (eval m1 (Division ((eval m2 Free), impl))) with
     | Division (l, r) -> (
        match (eval m1 (Division ((eval m2 l), r))) with
         | Division (l2, r2) -> r2
         | Key (Node(l2, r2)) -> (Key r2)
         | _ -> raise IMPOSSIBLE
     )
     | Key (Node(l, r)) -> (
        match (eval m1 (Division ((eval m2 (Key l)), (Key r)))) with
         | Division (l2, r2) -> r2
         | Key (Node(l2, r2)) -> (Key r2)
         | _ -> raise IMPOSSIBLE
     )
     | _ -> raise IMPOSSIBLE
    )

let getReady : map -> key list = fun m -> (
  try (
    let _ = (eval m Free) in
    let result = Hashtbl.fold (fun k v acc -> (shapeToKey v)::acc) nameToShapeTbl [] in
    let _ = (Hashtbl.clear nameToShapeTbl) in
    let _ = (Hashtbl.clear equalityPairsTbl) in removeDup result)
  with IMPOSSIBLE -> 
    (let _ = (Hashtbl.clear nameToShapeTbl) in
     let _ = (Hashtbl.clear equalityPairsTbl) in raise IMPOSSIBLE)
)


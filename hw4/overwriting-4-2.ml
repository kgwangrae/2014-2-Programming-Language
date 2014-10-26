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
            | Key of key (*No wildcard inside*)
            | Division of shape * shape

let nameToShapeTbl = Hashtbl.create 50

(*줄 수 줄이고 주석 영어번역해보자*)
(*더 넓은 조건을 바인딩하려하면 넘기는 게 맞음 호환 불가능한 걸 바인딩하려 들때 에러를 내야하는 거지*)
(*assign 순서에 의존적이면 안 되지. 그리고 name-shape binding을 어떤 이름에 대한 조건 추가? 라고 생각을 해보면, a조건에 b조건을 추가해서 범위를 좁히고 나서도 여전히 a조건은 만족하고 있을거야*)
let getShape : treasure -> shape = fun t ->
  Hashtbl.find nameToShapeTbl t

let rec print_s : shape -> string = fun s ->
  match s with
   | Free -> "Free"
   | Key (key) -> (match key with
     | Bar -> "Bar"
     | Node (k1,k2) -> 
         "("^(print_s (Key k1))^", "^(print_s (Key k2))^")"
     )
   | Division (s1, s2) ->
       "("^(print_s s1)^", "^(print_s s2)^")"

let rec print_t : treasure -> string = fun t ->
  match t with
   | StarBox -> "*"
   | NameBox (str) -> str

(* Bind new specific conditions to the current Name *)
let rec calcShape : shape -> shape -> shape = fun s_orig s_new ->
  match s_orig, s_new with
    | Free, _ -> s_new (* 'Free' type can mutate into anythig *)
    | _, Free -> s_orig (*If current condition is more specific than the new, don't change it*)
    | Division (l_orig, r_orig), Division (l_new, r_new) -> 
        Division ((calcShape l_orig l_new), (calcShape r_orig r_new))
    | Division (l_orig, r_orig), Key (Node (l_new, r_new)) -> 
        Division ((calcShape l_orig (Key(l_new))),(calcShape r_orig (Key(r_new))))
    | Key (Node (l_new, r_new)), Division (l_orig, r_orig) -> 
        Division ((calcShape (Key(l_new)) l_orig),(calcShape (Key(r_new)) r_orig))
    | Key (k_orig), Key (k_new) -> if (k_orig=k_new) then s_new else raise IMPOSSIBLE 
    | _, _ -> raise IMPOSSIBLE

(* Check equality : shapes should be mutually changeable *)
let isEqualable : shape -> shape -> bool = fun s1 s2 ->
  let rec isForceable : shape -> shape -> bool = fun s_orig s_new ->
  match s_orig, s_new with
    | Free, _ -> true
    | Division (l_orig, r_orig), Division (l_new, r_new) -> 
        (isForceable l_orig l_new) && (isForceable r_orig r_new)
    | Division (l_orig, r_orig), Key (Node (l_new, r_new)) -> 
        (isForceable l_orig (Key(l_new))) && (isForceable r_orig (Key(r_new)))
    | Key (Node (l_new, r_new)), Division (l_orig, r_orig) -> 
        (isForceable (Key(l_new)) l_orig) && (isForceable (Key(r_new)) r_orig)
    | Key (k_orig), Key (k_new) -> if (k_orig=k_new) then true else raise IMPOSSIBLE 
    | _, _ -> raise IMPOSSIBLE
  in
  (isForceable s1 s2) && (isForceable s2 s1)

let bindName : treasure -> shape -> bool = fun t s ->
  if (Hashtbl.mem nameToShapeTbl t)
  then (
    (*let _ = print_string ((print_t t)^" : ") in
    let _ = print_endline (print_s s) in*)
    ((Hashtbl.replace nameToShapeTbl t (calcShape (getShape t) s));true)
  )
  else (let _ = (Hashtbl.add nameToShapeTbl t s) in true)
  
let rec shapeToKey : shape -> key = fun s ->
  match s with 
  | Free -> Bar
  | Key k -> k
  | Division (l,r) -> Node ((shapeToKey l),(shapeToKey r))

(* m : map, 
 * impl : what shape this map should imply, determined by parent node of the map
 * This function verifies whether the designated shape is obtainable *)
let rec eval : map -> shape -> shape =
  fun m impl -> 
  match m with
  | End t -> (
     match t with 
     | StarBox -> (
        match impl with 
        | Free -> if (bindName t (Key(Bar))) then (Key(Bar)) else raise IMPOSSIBLE
        | Key(k) -> if (k=Bar) then (if (bindName t (Key(Bar))) 
                                     then impl else raise IMPOSSIBLE)
                               else raise IMPOSSIBLE
        | _ -> raise IMPOSSIBLE
       )
     | NameBox name -> if (bindName t impl) then impl else raise IMPOSSIBLE
    )
  | Guide (str,m_in) -> ( (*Simplify some codes here : remove Duplicates*)
     match impl with
     | Free -> (
        if (bindName (NameBox (str)) Free)
        then (Division (getShape (NameBox (str)), (eval m_in Free)))
        else raise IMPOSSIBLE
       )
     | Division (s1, s2) -> (
        if (bindName (NameBox (str)) s1)
        then (Division (getShape (NameBox (str)), (eval m_in s2)))
        else raise IMPOSSIBLE
        (* The namebox "str" may exist inside m_in, but it's not a must *)
       )
     | Key (Node(l,r)) -> (
        if (bindName (NameBox (str)) (Key (l)))
        then (Division ((Key (l)), (eval m_in (Key (r)))))
        else raise IMPOSSIBLE
       )
     | _ -> raise IMPOSSIBLE  
    )
  | Branch (m1, m2) -> (
    (* Free : Totally mutable type, like the 'Object' in Java *)
    (*의문 : 조금 더 까다로운 조건을 먼저 파싱하는 게 맞을까 *)
    (*까다로운 조건을 가지는 방향으로만 변화 가능하게 하기. 그리고 조건들의 제시 순서에는 의존하지 않는 게 맞는듯?*)
    match (eval m1 (Division (Free, impl))) with
     | Division (l, r) ->
       let s2 = eval m2 l in
       (match (eval m1 (Division (s2, r))) with
          | Division (l2, r2) -> if (isEqualable l2 s2) then r2 else raise IMPOSSIBLE
          | _ -> raise IMPOSSIBLE 
       )
     | _ -> raise IMPOSSIBLE
    )
  
  (*begin end 말고 _ 나 쓸걸 아래처럼 ㅡㅡ*)
    
let getReady : map -> key list = fun m -> (
  let _ = (eval m Free) in
  let result = Hashtbl.fold (fun k v acc -> (shapeToKey v)::acc) nameToShapeTbl [] in
  let _ = (Hashtbl.reset nameToShapeTbl) in
  removeDup result
)

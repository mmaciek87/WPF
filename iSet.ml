(*
Autor: Maciej Mioduszewski
Code review: Jakub Dziura
*)
type t = 
  | Empty
  | Node of t * (int * int) * t * int * int;;

let plus x y =
  if (x>=0)&&(y>=0) then
    if x > max_int - y then max_int
    else if y> max_int - x then max_int
    else x + y
  else x +y;;

let height set =
  match set with
  | Empty -> 0
  | Node(_,_,_,h,_) -> h;;

let dlugosc (p,k) =
  if k - p + 1 <= 0 then max_int
  else k - p +1;;

let rozmiar x =
  match x with 
  | Empty -> 0
  | Node(_,(p,k),_,_,n) -> n;; 

let make l (p,k) r =
  Node(l,(p,k),r, (max (height l) (height r)) + 1, plus (rozmiar l) (plus (dlugosc (p,k)) (rozmiar r)));;

let rec bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _,_) ->
        if height ll >= height lr then make ll lk (bal lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _,_) ->
              make (make ll lk lrl) lrk (bal lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _,_) ->
        if height rr >= height rl then make (bal l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _,_) ->
              make (bal l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r;;
  
let empty = Empty;;

let is_empty x =
  x = Empty;;

let rec min_elt x =
  match x with
  | Node(Empty,k,_,_,_) -> k
  | Node(l,_,_,_,_) -> min_elt l
  | Empty -> raise Not_found;;

let rec remove_min_elt = function
  | Node (Empty, _, r, _,_) -> r
  | Node (l, k, r, _,_) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt";;

let rec max_elt x =
  match x with
  | Node(_,k,Empty,_,_) -> k
  | Node(_,_,r,_,_) -> max_elt r
  | Empty -> raise Not_found;;

let rec remove_max_elt = function
  | Node (l, _, Empty, _,_) -> l
  | Node (l, k, r, _,_) -> bal l k (remove_max_elt r)
  | Empty -> invalid_arg "PSet.remove_min_elt";;

let merge x1 x2 =
    match (x1,x2) with
    | (Empty,Empty) -> Empty
    | (_,Empty) -> x1
    | (Empty,_) -> x2
    | _ -> 
      let k = min_elt x2 in
      bal x1 k (remove_min_elt x2);; 

(*Skleja dwa drzewa i przedział i ratuje z opresji w sytuacji gdy mamy przedziały np (1,3) (3,5) i skleja je w (1,5)*)
let rec sklej l (p,k) r =
    let nl = 
      if l = empty then empty
      else let (x,y) = max_elt l 
      in
        if abs(p-y) = 1 then 
          bal (remove_max_elt l) (x,k) empty
        else bal l (p,k) empty
    in 
      if nl = empty && r = empty then 
        make empty (p,k) empty
      else if r = empty then 
        merge nl empty
      else if nl = empty then 
        let (x,y) = min_elt r 
        in 
          if abs(x-k) = 1 then 
            bal empty (p,y) (remove_min_elt r)
          else bal empty (p,k) r
      else 
        let (x,y) = max_elt nl and (x2,y2) = min_elt r in
          if abs(x2-y) = 1 then 
            bal (remove_max_elt nl) (x,y2) (remove_min_elt r)
          else merge nl r;;


let rec join l ((a,b) as k) r =
  match (l,r) with
  | (Empty,_) -> sklej empty (a,b) r
  | (_,Empty) -> sklej l (a,b) empty
  | (Node(ll, lv, lr, lh,_), Node(rl, rv, rr, rh,_)) ->
    if lh > rh + 2 then bal ll lv (join lr k r) else
    if rh > lh + 2 then bal (join l k rl) rv rr else
    make l k r;;

let split n x =
  let rec loop n x =
    match x with
    | Empty -> (Empty,false,Empty)
    | Node (l,((p,k) as v),r,_,_) ->
      if (n>=p) && (n<=k) then
        let nl =
          if n = p then l else sklej l (p,n-1) empty
        and nr =
          if n = k then r else sklej empty (plus n 1,k) r
        in (nl,true,nr)
      else if n < p then
        let (ll, pres, rl) = loop n l in (ll, pres, join rl v r)
      else
        let (lr, pres, rr) = loop n r in (join l v lr, pres, rr)
    in loop n x;;

let remove (p,k) x =
  let (l,_,_) = split p x 
  and (_,_,r) = split k x 
  in merge l r;;

let add (p,k) x =
  match x with
  | Empty -> make empty (p,k) empty
  | _ ->
    let (l,_,_) = split p x
    and (_,_,r) = split k x
    in sklej l (p,k) r;;
  
let mem n x= 
  let rec szukaj n x =
    match x with
    | Empty -> false
    | Node(l,(p,k),r,h,_) ->
      if (n>=p) && (n<=k) then true
      else if n > k then 
        if r = Empty then false
          else szukaj n r
      else 
        if l = Empty then false
          else szukaj n l
  in szukaj n x;;

let iter f x=
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _,_) -> loop l; f k; loop r in
  loop x;;

let fold f x acc =
  let rec loop acc x= 
    match x with
    | Empty -> acc
    | Node (l, k, r, _,_) ->
          loop (f k (loop acc l)) r in
  loop acc x;;

let elements x = 
  let rec loop acc x = 
    match x with
    | Empty -> acc
    | Node (l,k,r,_,_) -> loop (k::(loop acc r)) l
  in loop [] x;;

let below n x =
  let rec pom a x =
    match x with
    | Empty -> a
    | Node(l,(p,k),r,_,_) ->
      if (n>=p)&&(n<=k)
        then (plus a (plus (rozmiar l) (dlugosc (p,n))))
      else if n < p
        then pom a l
      else 
        pom (plus a (plus (rozmiar l) (dlugosc(p,k)))) r 
  in pom 0 x;;

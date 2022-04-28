type 'a queue = 
  Node of 'a queue * 'a * 'a queue * int | Mt;;

let empty = Mt;;

exception Empty;;
 
let rec join d1 d2 =
  match (d1,d2) with 
  | (Mt,Mt) -> Mt
  | (Mt,_) -> d2
  | (_,Mt) -> d1
  | ((Node(ql1,w1,qr1,p1)),(Node(ql2,w2,qr2,p2))) ->
    if w1 > w2 then join d2 d1
    else let d3 = (join qr1 d2) in
    match ql1,d3 with
    | (Mt,Mt) -> Node(Mt,w1,Mt,1)
    | (Mt,_) -> Node(d3,w1,Mt,1)
    | (_,Mt) -> Node(ql1,w1,Mt,1)
    | Node(ql3,w3,qr3,p3),Node(ql4,w4,qr4,p4) ->
        if (p4>p3) then Node(d3,w1,ql1,p3+1) 
        else Node(ql1,w1,d3,p4+1);;

let delete_min a =
  match a with 
  | Mt -> raise Empty
  | (Node(q1,w1,q2,p1)) -> (w1, (join q1 q2));;
    
let add e a =
  join (Node(Mt,e,Mt,1)) a;;

let is_empty a =
  match a with
  | Mt -> true
  | Node(_,_,_,_) -> false;;


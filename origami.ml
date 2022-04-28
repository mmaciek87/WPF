open List;;

type point = float * float;;

type kartka = point -> int;;

let nalezy p (x,y) =
  if p >= x && p <= y
    then true else false;;

let epsilon = 0.000001;;

let rowne p x =
  if nalezy p (x-.epsilon,x+.epsilon) then true
  else false;;

let kwadrat x = x *. x;;

let prostokat (x1,y1) (x2,y2) = 
    function (x,y) ->
    if nalezy x (x1,x2) && nalezy y (y1,y2)
    then 1 else 0;;
  
let kolko (a,b) r =
  function (x,y) ->
  if kwadrat (x -. a) +. kwadrat (y -. b) <= r +. epsilon
    then 1 else 0;;

let daj (x1,y1) (x2,y2) =
  let a = (y1 -. y2)/.(x1 -. x2)
  in let b = y1 -. x1 *. a
  in (a,b);;

let symetryczny (x,y) (a,b) =
  let ap = -1. *. (1./.a)
  in 
  let bp = y -. ap *. x
  in
    let xp = (bp -. b)/.(a -. ap)
    in 
    let yp = a *. xp +. b
    in
      if x > xp && y < yp then
        (x -. 2. *. (x -. xp) , y +. 2. *. (yp -. y))
      else 
        ( x +. 2. *. (xp -. x),y -. 2. *. (y -. yp));;

let zloz (x1,y1) (x2,y2) k =
  function (x,y) as p ->
  let (a,b) = daj (x1,y1) (x2,y2) in
      if rowne x1 x2 then
        if y1 < y2 then
          if rowne x x1 then k p
          else if x > x1 then 0
          else k p + k (x1 +. (x1 -. x),y)
        else 
          if rowne x x1 then k p
          else if x > x1 then k p + k (x1 -. (x -. x1),y)
          else 0
      else if rowne y1 y2 then
        if x1 < x2 then
          if rowne y y1 then k p
          else if y > y1 then k p + k (x,y1 -. (y -. y1))
          else 0
        else 
          if rowne y y1 then k p
          else if y > y1 then 0 
          else k p + k (x,y1 -. (y -. y1))
      else if x1 < x2 then
        (
          if rowne y (a *. x +. b) then k p
          else if y -. a *. x > b then k p + k (symetryczny p(a,b))
          else 0
        )
      else 
      (
        if rowne y (a *. x +. b) then k p
        else if y -. a *. x < b then k p + k (symetryczny p (a,b))
        else 0
      );;

let skladaj l k = fold_left (fun a (p1, p2) -> zloz p1 p2 a) k l;; 
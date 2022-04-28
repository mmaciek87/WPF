type przedzial ={typ: string; first: float; second: float};;

let zrob_przedzial a x y =
	{typ = a; first = x; second = y};;

let przeciwne x= 
	if x.typ = "p" then zrob_przedzial "p" ((-1.)*.x.second) ((-1.)*.x.first)
else zrob_przedzial "d" ((-1.)*.x.second) ((-1.)*.x.first);;

let czy_nan x =
	if classify_float x.first = FP_nan || classify_float x.second = FP_nan then true
else false;;

let wartosc_dokladnosc x p =
	zrob_przedzial "p" (min (x -. p*.x/.100.) (x +. p*.x/.100.)) (max(x -. p*.x/.100.) (x +. p*.x/.100.));;

let wartosc_od_do x y =
	zrob_przedzial "p" x y;;

let wartosc_dokladna x = 
	wartosc_od_do x x;; 

let in_wartosc x y =
	if x.typ = "p" then 
		(if (y >= x.first && y <= x.second) then true else false)
	else 
		(if (y > x.first && y < x.second) then false else true);;

let min_wartosc x =
	if x.typ = "p" then x.first
else neg_infinity;;

let max_wartosc x = 
	if x.typ = "p" then x.second
else infinity;;

let sr_wartosc x =
	if x.typ = "p" then
		 (if (x.first = neg_infinity && x.second = infinity) then nan else (x.first +. x.second)/.2.)
else nan;;

let plus x y =
	if czy_nan x || czy_nan y then zrob_przedzial "p" nan nan
	else if (x.typ = "p" && y.typ = "p") then 
		zrob_przedzial "p" (x.first +. y.first) (x.second +. y.second)
	else if (x.typ = "p" && y.typ = "d") then
		(
			if y.first +. x.second > y.second +. x. first 
			then zrob_przedzial "p" neg_infinity infinity
				else zrob_przedzial "d" (y.first +. x.second) (y.second +. x.first)
		)
	else if (x.typ = "d" && y.typ = "p") then
		(
			if x.first +. y.second > x.second +. y. first 
			then zrob_przedzial "p" neg_infinity infinity 
				else zrob_przedzial "d" (x.first +. y.second) (x.second +. y.first)
		)
	else zrob_przedzial "p" neg_infinity infinity;;

let minus x y = 
	if czy_nan x || czy_nan y then zrob_przedzial "p" nan nan
	else plus x (przeciwne y);;

let rec razy x y =
	if czy_nan x || czy_nan y then zrob_przedzial "p" nan nan
	else if x = wartosc_dokladna(0.) || y = wartosc_dokladna (0.) then zrob_przedzial "p" 0. 0.
	else if x.typ = "p" && y.typ ="p" then
		let rec minmax w l =
			match l with
			| [] -> w
			| e::t -> minmax (zrob_przedzial "p" (min e w.first) (max e w.second)) (t)
		in minmax (zrob_przedzial "p" infinity neg_infinity) [x.first *. y.first; x.first *. y.second; x.second *. y.first; x.second *. y.second]
	else if x.typ = "d" && y.typ = "p" then 
	(
		if in_wartosc y 0. then zrob_przedzial "p" neg_infinity infinity
		else  if y.first > 0. then
		(
		let a = max (x.first *. y.first) (x.first *. y.second) in
		let b = min (x.second *. y.first) (x.second *. y.second) in
		if a >= b then zrob_przedzial "p" neg_infinity infinity else zrob_przedzial "d" a b
		)
		else
		(
		let b = min (x.first *. y.first) (x.first *. y.second) in
		let a = max (x.second *. y.first) (x.second *. y.second) in
		if a >= b then zrob_przedzial "p" neg_infinity infinity else zrob_przedzial "d" a b
		)
	)
	else if x.typ = "p" && y.typ ="d" then razy y x 
	else 
		(
		if in_wartosc y 0. || in_wartosc x 0. then zrob_przedzial "p" neg_infinity infinity
		else 
		(
			let a = max (x.first *. y.second) (x.second *. y.first) in 
			let b = min (x.first *. y.first) (x.second *. y.second) in
			zrob_przedzial "d" a b
		)
		);;


let podzielic x y =
	if czy_nan x || czy_nan y then zrob_przedzial "p" nan nan
	else if y.typ = "d" then 
		(
		if in_wartosc y 0. then
		 razy (x) (zrob_przedzial "d" (1./.y.second) (1./.y.first))
			else razy (x) (zrob_przedzial "p" (1./.y.first) (1./.y.second)) 
		)
	else if y.first = 0. && y.second = 0. then zrob_przedzial "p" nan nan
	else if y.first = 0. && y.second <> 0. then razy (x) (zrob_przedzial "p" (1./.y.second) infinity)
	else if y.first <> 0. && y.second = 0. then razy (x) (zrob_przedzial "p" neg_infinity (1./.y.first))
	else if in_wartosc y 0. = false then razy (x) (zrob_przedzial "p" (1./.y.second) (1./.y.first))
	else if in_wartosc y 0. then razy (x) (zrob_przedzial "d" (1./.y.first) (1./.y.second))
	else zrob_przedzial "p" neg_infinity infinity;;



		


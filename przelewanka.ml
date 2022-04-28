(*
Autor: Maciej Mioduszewski
Code review: Michał Kimborowicz 
*)
let przelewanka tab =
  let n = Array.length tab in
  let szklanki = Array.init n (fun i -> fst(tab.(i))) in
  let cel = Array.init n (fun i -> snd(tab.(i))) in
 
  let wylej t i =
    let pom = Array.copy t in
    pom.(i) <- 0;
    pom;
  in 

  let wlej t i =
    let pom = Array.copy t in
    pom.(i) <- szklanki.(i);
    pom;
  in
  
  let przelej t i j =
    let pom = Array.copy t in
      if (t.(i) + t.(j) <= szklanki.(j)) then
      begin
        pom.(j) <- (t.(i) + t.(j));
        pom.(i) <- 0;
      end;
      if (t.(i) + t.(j) > szklanki.(j)) then
      begin
        pom.(j) <- szklanki.(j);
        pom.(i) <- (t.(i) - (szklanki.(j) - t.(j)));
      end;
    pom;
  in

  let rec nwd a b = 
    if b = 0 then a 
    else nwd b (a mod b)
  in
  
  let warunek1 =
    let bol = ref true in
    let a = ref 0 in
    for i = 0 to n - 1 do
      a := nwd !a (szklanki.(i))
    done;
    if !a <> 0 then 
    (
      for i = 0 to n - 1 do
        if (cel.(i) mod !a <> 0) then bol := false;
      done;
    );
    !bol;
  in
  
  let warunek2 =
    let bol = ref false in
      for i = 0 to n - 1 do
        if (szklanki.(i) = cel.(i) || cel.(i) = 0) 
          then bol := true;
      done;
    !bol;
  in

  if n = 0 then 0
  else if (not (warunek1 && warunek2)) then (-1)
  else
  (
    let htbl = Hashtbl.create 1000007 in
    let q = Queue.create () in
    let stan = Array.make n 0 in
    let bol = ref false in
    let sprawdz stan krok =
      Queue.push stan q;
      Hashtbl.add htbl stan (krok + 1);
    in
      Hashtbl.add htbl stan 0;
      Queue.add stan q;
      while (not (Queue.is_empty q) && !bol = false) do
        let astan = Queue.pop q in
        let krok = Hashtbl.find htbl astan in
        if (Hashtbl.mem htbl cel) then 
        (
          bol := true;
        );
        for i = 0 to n - 1 do
          let pom1 = wylej astan i in
          let pom2 = wlej astan i in
          if (not (Hashtbl.mem htbl pom1)) then
          (
            sprawdz pom1 krok;
          );
          if (not (Hashtbl.mem htbl pom2)) then
          (
            sprawdz pom2 krok;
          );
          for j = 0 to n -1 do
            if (i <> j) then
            (
              let pom3 = przelej astan i j in
              if (not (Hashtbl.mem htbl pom3)) then
              (
                sprawdz pom3 krok;
              );
            )
          done;
        done;
      done;
    if (Hashtbl.mem htbl cel)
      then (Hashtbl.find htbl cel)
    else (-1)
    );;
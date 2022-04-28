(*Autor: Maciej Mioduszewski*)
(*Code review: Jan Wangrat grupa 4*)

open PMap;;

exception Cykliczne;;
  
let topol l =
  let wyn = ref [] in
  let g = ref
    (List.fold_left (fun a (v,lista) -> 
    if exists v a then 
      let (lista2,_) = find v a
      in remove v a; add v (ref(!lista2 @ lista), ref 0) a
    else add v (ref lista, ref 0) a
  ) empty l)
  in let rec dfs v (lista,odw) = 
      match !odw with
      | 2 -> raise Cykliczne 
      | 1 -> ()
      | 0 ->
        odw := 2;
        while !lista <> [] do
          let v = List.hd (!lista) in
            if exists v !g then dfs v (find v !g);
            if exists v !g = false then
              begin
                wyn := v ::(!wyn);
                g := add v (ref [], ref 1) !g;
              end;
          lista := List.tl (!lista);
        done;
        wyn := v :: (!wyn);
        odw := 1;
    in iter dfs !g;
  !wyn;;
  

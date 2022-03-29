let range_loop n m =
  let lst = ref [] in 
  let ctr = ref n in 
  while !ctr <= m do 
    lst := !ctr :: !lst;
    ctr := !ctr + 1;
  done;
  List.rev !lst;;

let range n m =
  let rec range_tr a b acc =
    if a >= b then acc 
    else range_tr (a + 1) b (a :: acc) in 
  List.rev (range_tr n m []);;

let rec map_mutable (lst : 'a mlist ref) (f : 'a -> 'a) =
  match !lst with
  | Nil -> []
  | Cons (ele, tail_ref) -> 
    lst := Cons(f ele, tail_ref);
    map_mutable f tailref;;





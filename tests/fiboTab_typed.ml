let fibo (n : int) : int =
  if n <= 1 then 1 else (
    let tab : int vect = aMake n in
    tab.(0) <- 1;
    tab.(1) <- 1;
    let rec aux (i : int) : int =
      if i=n then tab.(i-1) + tab.(i-2) else (
        tab.(i) <- tab.(i-1) + tab.(i-2);
        aux (i+1)
        )
      in
    aux 2
  )
  in
  prInt fibo 8;;

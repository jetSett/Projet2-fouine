let fibo n =
  let tab = aMake n in
  tab.(0) <- 1;
  tab.(1) <- 1;
  if n <= 1 then 1 else (
  let rec aux i =
    if i=n then tab.(i-1) + tab.(i-2) else (
      tab.(i) <- tab.(i-1) + tab.(i-2);
      aux (i+1)
      )
    in
    aux 2
  )
  in
fibo 15;;

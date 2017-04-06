let fibonacci n =
  let a = ref 1 in
  let b = ref 1 in
  let rec f n =
    if n <= 1
    then 0
    else (
      let c = !a in
      a := !b;
      b := !a + c;
      f (n-1)
    )
  in f n; !b;;

prInt (fibonacci 5);;

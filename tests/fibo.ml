let fibonacci n =
  let a = ref 0 in
  let b = ref 1 in
  let rec f n =
    if n <= 1
    then ()
    else (
      let c = !a in
      a := !b;
      b := !a + c;
      f (n-1)
    )
  in f n; !b;;

prInt (fibonacci 25);;

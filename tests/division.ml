let modulo x y =
  if y = 0 || x < 0 || y < 0 then raise (E -424242)
  else
    let rec test = fun x ->
      if x < y then x
      else test (x - y)
    in
    test x;;

(try
  prInt (modulo 6 14);
  prInt (modulo 25 5);
  prInt (modulo 112 3);
  prInt (modulo 19 0)
with E error_code -> prInt error_code);

let pi = 314159 in modulo pi 42;;

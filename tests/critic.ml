try
  let a = prInt 42 in
  let b = raise (E 1) in
  raise (E 2)
with E x -> prInt x;;

let rec test n =
  if n<>0 then test ((prInt n) -1) else 0
  in
  test 10;;

try
  try
    try raise (E 42)
    with E x -> raise (E (x+1))
  with E y -> prInt y
with E y -> prInt 1337;;

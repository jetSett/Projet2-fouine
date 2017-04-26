try
  prInt 42;
  raise (E 1);
  prInt 1337
  raise (E 2);
  prInt 314;
  raise (E 3);
  prInt 123456
with E x -> prInt x;;

try
  prInt 42;
  raise 1;
  prInt 1337;
  raise 2;
  prInt 314;
  raise 3;
  prInt 123456
with E x -> prInt x;;

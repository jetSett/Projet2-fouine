let t = aMake 5 in
t.(2) <- 5;
t.(2);
t.(3) <- 8;
t.(1) <- t.(1+1) + t.(4-1);
prInt t.(0);
prInt t.(1);
prInt t.(2);
prInt t.(3);
prInt t.(4);
t;;

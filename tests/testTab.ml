let t = aMake 5 in
t.(2) <- 5;
t.(2);
t.(3) <- 8;
t.(0) <- t.(1+1) + t.(4-1);
prInt t.(0);
t;;

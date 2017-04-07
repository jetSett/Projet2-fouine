let compose f x = f (f x);;
let addOne x = x + 1;;
let addTwo = compose addOne;;

prInt (addTwo 0);;

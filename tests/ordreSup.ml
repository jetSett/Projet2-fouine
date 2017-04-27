let composeTwice f x = f (f x);;
let addOne x = x + 1;;
let addTwo = composeTwice addOne;;

prInt (addOne 0);;

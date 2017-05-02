let f g h x = (g (h x));;
let a = fun x -> x+42;;
let b = fun y -> y*2;;
let i = f a b;;
i 1;;

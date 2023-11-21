Control.Print.printDepth := 100;
Control.Print.printLength := 100;

(* Problem 1 *)

fun merge [] L = L
    | merge L [] = L
    | merge (h1::rest1) (h2::rest2) = 
    if (h1 < h2) then h1 :: merge rest1 (h2::rest2)
    else h2 :: merge (h1::rest1) rest2;

(* Call Problem 1 *)
merge [1,3,5,7,9] [2,4,6,8,10] ;

(* Problem 2 *)
fun split [] = ([], [])
    | split [x] = ([x], [])
    | split (first::second::rest) = 
        let
            val (split1, split2) = split(rest)
        in
            ((first::split1), (second::split2))
        end;

(* Call Problem 2 *)
split [1,4,2,6,8,3,9,5,4];

(* Problem 3 *)


(* Call Problem 3 *)
mergeSort [1,7,2,6,8,3,9,5,4];


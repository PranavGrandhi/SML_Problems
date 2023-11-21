(* Pranav Venkata Grandhi *)
(* pvg2018 *)
(* Standard ML Assignment *)

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
            val (split1, split2) = split rest
        in
            ((first::split1), (second::split2))
        end;

(* Call Problem 2 *)
split [1,4,2,6,8,3,9,5,4];

(* Problem 3 *)
fun mergeSort [] = []
    | mergeSort [x] = [x]
    | mergeSort L = 
    let
        val (split1, split2) = split L
    in
        merge (mergeSort split1) (mergeSort split2)
    end;

(* Call Problem 3 *)
mergeSort [1,7,2,6,8,3,9,5,4];

(* Problem 4 *)
fun sort (op <) [] = []
    | sort (op <) [x] = [x]
    | sort (op <) L = 
        let
            fun merge [] L = L
                | merge L [] = L
                | merge (h1::rest1) (h2::rest2) = 
                if (h1 < h2) then h1 :: merge rest1 (h2::rest2)
                else h2 :: merge (h1::rest1) rest2

            fun split [] = ([], [])
                | split [x] = ([x], [])
                | split (first::second::rest) = 
                    let
                        val (split1, split2) = split rest
                    in
                        ((first::split1), (second::split2))
                    end

            val (split1, split2) = split L
        in 
            merge (sort (op <) split1) (sort (op <) split2)
        end;
            
(* Call Problem 4 *)
sort (op <) [1,9, 3, 6, 7];
sort (fn(a,b) => length a < length b) [[1, 9, 3, 6], [1], [2,4,6], [5,5]];

(* Problem 5 *)
datatype 'a tree = node of 'a * 'a tree * 'a tree | leaf of 'a | empty ; 

(* Call Problem 5 *)
val tree1 = node (5, node (4, leaf 3, empty),
                    node (8, node (7, leaf 6, empty),
                            node (9, empty, leaf 10)));

(* Problem 6 *)
fun labels (empty) = []
    | labels (leaf l) = [l]
    | labels (node (root, left_child, right_child)) =
        labels left_child @ (root :: labels right_child); 

(* Call Problem 6 *)
labels tree1;

(* Problem 7 *)
infix ==
fun replace (op ==) x y (empty) = (* If empty *)
        empty
    | replace (op ==) x y (leaf l) = (* If leaf *)
        if l == x then
            (leaf y)
        else
            (leaf l)
    | replace (op ==) x y (node(root, left_child, right_child)) = (* If node *)
        let
            val new_root = (* define root *)
                if root == x then
                    y
                else
                    root
            val new_left_child = (* define new left child *)
                replace (op ==) x y left_child
            val new_right_child = (* define new right child *)
                replace (op ==) x y right_child
        in
            node(new_root, new_left_child, new_right_child)
        end;    

(* Call Problem 7 *)
val tree2 = replace (op =) 4 40 tree1;
labels tree2;
val tree3 = replace (op <>) 7 0 tree1;
labels tree3;

(* Problem 8 *)
fun replaceEmpty y (empty) = y
    | replaceEmpty y (leaf l) = (leaf l)
    | replaceEmpty y (node(root, left_child, right_child)) =
        let
            val new_root = root
            val new_left_child = replaceEmpty y left_child
            val new_right_child = replaceEmpty y right_child
        in
            node(new_root, new_left_child, new_right_child)
        end;

(* Call Problem 8 *)
val tree4 = replaceEmpty (node (12, leaf 11, leaf 13)) tree1;
labels tree4;

(* Problem 9 *)

(* Increment function copied from question *)
fun increment empty = leaf 0
    | increment (leaf a) = leaf (a+1)
    | increment (node (a, L, R)) = node (a+1, L, R);

(* MapTree function *)
fun mapTree f (empty) = f empty
    | mapTree f (leaf l) = f (leaf l)
    | mapTree f (node(root, left_child, right_child)) =
        let
            val new_left_child = mapTree f left_child
            val new_right_child = mapTree f right_child
        in
            f (node(root, new_left_child, new_right_child))
        end;

(* Call Problem 9 *)
val tree5 = mapTree increment tree1;
labels tree5;


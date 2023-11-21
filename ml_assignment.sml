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

(* Problem 2 *)
fun split [] = ([], [])
    | split [x] = ([x], [])
    | split (first::second::rest) = 
        let
            val (split1, split2) = split rest
        in
            ((first::split1), (second::split2))
        end;

(* Problem 3 *)
fun mergeSort [] = []
    | mergeSort [x] = [x]
    | mergeSort L = 
    let
        val (split1, split2) = split L
    in
        merge (mergeSort split1) (mergeSort split2)
    end;

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
            
(* Problem 5 *)
datatype 'a tree = node of 'a * 'a tree * 'a tree | leaf of 'a | empty ; 

(* Problem 6 *)
fun labels (empty) = []
    | labels (leaf l) = [l]
    | labels (node (root, left_child, right_child)) =
        labels left_child @ (root :: labels right_child); 

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
            val new_root = root
            val new_left_child = mapTree f left_child
            val new_right_child = mapTree f right_child
        in
            f (node(new_root, new_left_child, new_right_child))
        end;

(* Problem 10 *)
infix <
fun sortTree (op <) T = mapTree (fn 
    leaf l => leaf (sort (op <) l) 
  | node (root, left_child, right_child) => node (sort (op <) root, left_child, right_child)
  | empty => empty) T;
(* Call Problem 1 *)
merge [1,3,5,7,9] [2,4,6,8,10] ;

(* Call Problem 2 *)
split [1,4,2,6,8,3,9,5,4];

(* Call Problem 3 *)
mergeSort [1,7,2,6,8,3,9,5,4];

(* Call Problem 4 *)
sort (op <) [1,9, 3, 6, 7];
sort (fn(a,b) => length a < length b) [[1, 9, 3, 6], [1], [2,4,6], [5,5]];

(* Call Problem 5 *)
val tree1 = node (5, node (4, leaf 3, empty),
                    node (8, node (7, leaf 6, empty),
                            node (9, empty, leaf 10)));

(* Call Problem 6 *)
labels tree1;

(* Call Problem 7 *)
val tree2 = replace (op =) 4 40 tree1;
labels tree2;
val tree3 = replace (op <>) 7 0 tree1;
labels tree3;

(* Call Problem 8 *)
val tree4 = replaceEmpty (node (12, leaf 11, leaf 13)) tree1;
labels tree4;

(* Call Problem 9 *)
val tree5 = mapTree increment tree1;
labels tree5;

(* Call Problem 10 *)
val tree6 = node ([1,5,6,8],leaf [1,2,3,4], 
                        node ([12,4,16,13], empty, leaf [0,2,5,7])) ;
labels tree6;
val tree7 = sortTree (op <) tree6;
labels tree7;
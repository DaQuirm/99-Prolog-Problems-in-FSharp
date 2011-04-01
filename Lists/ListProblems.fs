module ListProblems

(* Problem 1.01 *)
// Find the last element of a list.
let lastElement list = 
    List.rev list |> List.head

(* Problem 1.02 *)
// Find the last but one element of a list.
let lastButOneElement list = 
    List.rev list |> List.tail |> List.head

(* Problem 1.03 *)    
// Find the K'th element of a list. 
// The first element in the list is number 1.
let kthElement k list = 
    List.nth list (k-1)

(* Problem 1.04 *)
// Find the number of elements of a list.
let listLength list = 
    list |> List.length

(* Problem 1.05 *)
// Reverse a list.
let listReverse list = 
    list |> List.rev

(* Problem 1.06 *)
// Find out whether a list is a palindrome. 
// A palindrome can be read forward or backward; e.g. [x,a,m,a,x].
let isListPalindrome list = 
    List.rev list = list

(** Problem 1.07 **)
// Flatten a nested list structure.
// Transform a list, possibly holding lists as elements into a 'flat' list 
// by replacing each list with its elements (recursively).

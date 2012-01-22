module ListProblems

(* Problem 1.01 *)
// Find the last element of a list.
let lastElement list = 
    List.rev list |> List.head

(* Problem 1.02 *)
// Find the last but one element of a list.
// (de: zweitletztes Element, fr: avant-dernier élément)
// DaQuirm's comment: (en: penultimate element)
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
// Hint: Use the predefined predicates is_list/1 and append/3
type NestedList<'T> = 
    | Element of 'T
    | List of NestedList<'T> list

let rec listFlatten list = 
    [for item in list do 
        match item with 
        | List l -> yield! listFlatten l
        | Element e -> yield e
    ]

(** Problem 1.08 **) 
// Eliminate consecutive duplicates of list elements. 
// If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
let compress list =
    List.foldBack (fun item acc -> 
        match acc with
        | [] -> [item]
        | head :: _ when head <> item -> item :: acc
        | _ -> acc
    ) list []

(** Problem 1.09 **)
// Pack consecutive duplicates of list elements into sublists. 
// If a list contains repeated elements they should be placed in separate sublists.
let pack list =
    List.foldBack (fun item acc -> 
        match acc with
        | [] -> [[item]]
        | headList :: _ when headList.Head <> item -> [item] :: acc
        | headList :: tail when headList.Head = item -> (item :: headList) :: tail
        | _ -> acc
    ) list []

(* Problem 1.10 *)
// Run-length encoding of a list. 
// Use the result of problem 1.09 to implement the so-called run-length encoding data compression method. 
// Consecutive duplicates of elements are encoded as terms [N,E] where N is the number of duplicates of the element E.
let encode list =
    list |> (pack >> List.map (fun list -> list.Length, list.Head))

(* Problem 1.11 *)
// Modified run-length encoding.
// Modify the result of problem 1.10 in such a way that if an element has no duplicates it is simply copied into the result list.
// Only elements with duplicates are transferred as [N,E] terms.
type RleList<'T> =     
    | Single of 'T
    | Tuple of int * 'T 

let encode_modified list =
    list |> pack |> List.map (fun list -> 
        match list.Length with
        | 1 -> Single list.Head
        | _ -> Tuple (list.Length, list.Head)
    )

(** Problem 1.12 **)
// Decode a run-length encoded list. 
// Given a run-length code list generated as specified in problem 1.11. Construct its uncompressed version.
let decode list =
    List.collect (fun item ->
        match item with
        | Single value -> [value]
        | Tuple (length, value) -> List.replicate length value
    ) list

(** Problem 1.13 **) 
// Run-length encoding of a list (direct solution).
// Implement the so-called run-length encoding data compression method directly.
// I.e. don't explicitly create the sublists containing the duplicates, as in problem 1.09, but only count them.
// As in problem 1.11, simplify the result list by replacing the singleton terms [1,X] by X.
let encode_direct list =
    List.foldBack (fun item acc -> 
        match acc with
        | [] -> [Single item]
        | head :: tail ->
            match head with
            | Single value when value = item -> Tuple (2, value) :: tail
            | Tuple (length, value) when value = item -> Tuple (length + 1, value) :: tail
            | _ -> Single item :: acc
    ) list [] 

(* Problem 1.14 *)
// Duplicate the elements of a list.
let dupli list =
    List.collect (fun item -> [item; item]) list

(** Problem 1.15 **)
// Duplicate the elements of a list a given number of times.
let ndupli n list = 
    List.collect (fun item -> List.replicate n item) list

(** Problem 1.16 **) 
// Drop every N'th element from a list.
let drop n list =
    list |> List.mapi (fun index item -> item, index)
         |> List.filter (fun (item, index) -> (index + 1) % n <> 0)
         |> List.map fst

(* Problem 1.17 *)
// Split a list into two parts; the length of the first part is given. 
// Do not use any predefined predicates.
let split length list =
    list |> List.mapi (fun index item -> item, index)
         |> List.partition (fun (item, index) -> index < length)
         |> (fun (left, right) -> ((left |> List.map fst), (right |> List.map fst)))

(** Problem 1.18 **)
// Extract a slice from a list.
// Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list
// (both limits included). Start counting the elements with 1.
let slice i k list =
    (Seq.skip (i - 1) >> Seq.take (k - i + 1) >> Seq.toList) list

(** Problem 1.19 **)
// Rotate a list N places to the left.
// Hint: Use the predefined predicates length/2 and append/3, as well as the result of problem 1.17.
let rotate n list =
    let length = List.length list
    split ((n + length) % length) list |> (fun (left, right) -> right @ left)

(* Problem 1.20 *)
// Remove the K'th element from a list.
let remove_at position list =
    (split (position - 1) list) |> (fun (left, right) -> right.Head, left @ right.Tail)

(* Problem 1.21 *)
// Insert an element at a given position into a list.
let insert_at item position list =
    (split (position - 1) list) |> (fun (left, right) -> left @ item :: right)

(* Problem 1.22 *)
// Create a list containing all integers within a given range.
let range first last =
    [first..last]

(** Problem 1.23 **)
// Extract a given number of randomly selected elements from a list. 
// The selected items shall be put into a result list.
// Hint: Use the built-in random number generator random/2 and the result of problem 1.20.
let rnd = new System.Random();

let rnd_select n list =
    List.sortWith (fun item1 item2 -> rnd.Next(0, 2) * 2 - 1) list
    |> Seq.take n |> Seq.toList

(* Problem 1.24 *)
// Lotto: Draw N different random numbers from the set 1..M.
// Hint: Combine the solutions of problems 1.22 and 1.23.
let lotto_select n first last =
    rnd_select n [first..last]

(* Problem 1.25 *)
// Generate a random permutation of the elements of a list.
// Hint: Use the solution of problem 1.23.
let rnd_permu list =
    List.sortWith (fun item1 item2 -> rnd.Next(0, 2) * 2 - 1) list

(** Problem 1.26 **)
// Generate the combinations of K distinct objects chosen from the N elements of a list
// In how many ways can a committee of 3 be chosen from a group of 12 people?
// We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients).
// For pure mathematicians, this result may be great. But we want to really generate all the possibilities (via backtracking).
//TODO: improve by adding a more general match pattern
let rec combination m list =
    match (m, list) with
    | (0,_) -> [[]]
    | (_,[]) -> []
    | (n, head :: tail) -> List.map (fun item -> head :: item) (combination (n-1) tail) @ combination n tail

(** Problem 1.27 **)
// Group the elements of a set into disjoint subsets. 
// a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? 
// Write a predicate that generates all the possibilities via backtracking.
// b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
// Note that we do not want permutations of the group members; 
// i.e. [[aldo,beat],...] is the same solution as [[beat,aldo],...]. 
// However, we make a difference between [[aldo,beat],[carla,david],...] and [[carla,david],[aldo,beat],...].
let rec sieve_combination m alist plist =
    match (m, alist) with
    | (0,_) -> [[], plist @ alist]
    | (_,[]) -> []
    | (n, head::tail) -> List.map (fun (comb, unused) -> (head :: comb, unused)) 
                                  (sieve_combination (n-1) tail plist) @ sieve_combination n tail (head :: plist)

let rec group_comb groups list =
    match groups with
    | [] -> [[]]
    | head :: tail -> 
        (sieve_combination head list []) 
        |> List.collect (
            fun (comb, unused) -> 
                List.map (fun solution -> comb :: solution) (group_comb tail unused)
        )

let group234 list = group_comb [2;3;4] list

(** Problem 1.28a **)
// Sorting a list of lists according to length of sublists
// a) We suppose that a list (InList) contains elements that are lists themselves.
// The objective is to sort the elements of InList according to their length. 
// E.g. short lists first, longer lists later, or vice versa.
let lsort list =
    List.sortBy (fun item -> List.length item) list

(** Problem 1.28b **)
// b) Again, we suppose that a list (InList) contains elements that are lists themselves.
// But this time the objective is to sort the elements of InList according to their length frequency;
// i.e. in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.
let lfsort list =
    Seq.groupBy List.length list 
    |> Seq.map (snd >> List.ofSeq) 
    |> Seq.sortBy Seq.length 
    |> List.concat;;

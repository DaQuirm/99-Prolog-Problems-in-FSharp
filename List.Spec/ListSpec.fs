module ListSpec

open NaturalSpec
open ListProblems

(* Problem 1.01 *)
[<Scenario>]
let ``When calculating lastElement of ['a';'b';'c';'d'] it should equal 'd' ``() =
  Given ['a';'b';'c';'d']                 
    |> When calculating lastElement
    |> It should equal 'd'          
    |> Verify

[<Scenario>]
let ``When calculating lastElement of ['k';'j';'h';'g';'f'] it should equal 'f' ``() =
  Given ['a';'b';'c';'d']                 
    |> When calculating lastElement
    |> It should equal 'd'          
    |> Verify


(* Problem 1.02 *)
[<Scenario>] //test by nash (http://github.com/nashby)
let ``When calculating lastButOneElement of [1;2;3;4;5;6;7;8;9] it should equal 8 ``() =
  Given [1;2;3;4;5;6;7;8;9]                
    |> When calculating lastButOneElement
    |> It should equal 8          
    |> Verify

[<Scenario>]
let ``When calculating lastButOneElement of [0..3..24] it should equal 21 ``() =
  Given [0..3..24] 
    |> When calculating lastButOneElement
    |> It should equal 21          
    |> Verify


(* Problem 1.03 *)
[<Scenario>] 
let ``When calculating kthElement of 3 and ['a';'b';'c';'d';'e'] it should equal 'c' ``() =
  Given ['a';'b';'c';'d';'e']              
    |> When calculating kthElement 3
    |> It should equal 'c'          
    |> Verify


(* Problem 1.04 *)
[<Scenario>] //test by nash (http://github.com/nashby)
let ``When calculating listLength of [1;2;3;4;5;6;7;8;9] it should equal 9 ``() =
  Given [1;2;3;4;5;6;7;8;9]                
    |> When calculating listLength
    |> It should equal 9          
    |> Verify

[<Scenario>] //test by miksayer (http://github.com/miksayer)
let ``When calculating listLength of [1;2;3] it should equal 3 ``() =
  Given [1;2;3]                
    |> When calculating listLength
    |> It should equal 3
    |> Verify


(* Problem 1.05 *)
[<Scenario>] 
let ``When calculating listReverse of [1..5] it should equal [5;4;3;2;1] ``() =
  Given [1..5]                
    |> When calculating listReverse
    |> It should equal [5;4;3;2;1]
    |> Verify


(* Problem 1.06 *)
[<Scenario>] 
let ``When calculating isListPalindrome of ['x';'a';'m';'a';'x'] it should equal true``() =
  Given ['x';'a';'m';'a';'x']               
    |> When calculating isListPalindrome
    |> It should equal true
    |> Verify

[<Scenario>]  //test by nash (http://github.com/nashby)
let ``When calculating isListPalindrome of [1;2;3;4;4;3;2;1] it shouldn't equal false``() =
  Given [1;2;3;4;4;3;2;1]
    |> When calculating isListPalindrome
    |> It shouldn't equal false
    |> Verify

[<Scenario>]  //test by miksayer (http://github.com/miksayer)
let ``When calculating isListPalindrome of [1;4;2;3;2;2;1] it should equal false``() =
  Given [1;4;2;3;2;2;1]
    |> When calculating isListPalindrome
    |> It should equal false
    |> Verify

(** Problem 1.07 **)
[<Scenario>]
let ``When flattening list [a, [b, [c, d], e]] with listFlatten it should equal [a, b, c, d, e]``() =
  Given [Element 'a'; List[Element 'b'; List[Element 'c'; Element 'd']; Element 'e']]
    |> When calculating listFlatten
    |> It should equal ['a'; 'b'; 'c'; 'd'; 'e']
    |> Verify

(** Problem 1.08 **)
[<Scenario>]
let ``When removing consecutive duplicates of the list [a,a,a,a,b,c,c,a,a,d,e,e,e,e] with compress it should equal [a,b,c,a,d,e]``() =
  Given ['a';'a';'a';'a';'b';'c';'c';'a';'a';'d';'e';'e';'e';'e']
    |> When calculating compress
    |> It should equal ['a';'b';'c';'a';'d';'e']
    |> Verify

(** Problem 1.09 **)
[<Scenario>]
let ``When packing list [a,a,a,a,b,c,c,a,a,d,e,e,e,e] with pack it should equal [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]``() =
  Given ['a';'a';'a';'a';'b';'c';'c';'a';'a';'d';'e';'e';'e';'e']
    |> When calculating pack
    |> It should equal [['a';'a';'a';'a'];['b'];['c';'c'];['a';'a'];['d'];['e';'e';'e';'e']]
    |> Verify

(* Problem 1.10 *)
[<Scenario>]
let ``When RLE-encoding list [a,a,a,a,b,c,c,a,a,d,e,e,e,e] with encode it should equal [[4,a],[1,b],[2,c],[2,a],[1,d][4,e]]``() =
  Given ['a';'a';'a';'a';'b';'c';'c';'a';'a';'d';'e';'e';'e';'e']
    |> When calculating encode
    |> It should equal [(4,'a');(1,'b');(2,'c');(2,'a');(1,'d');(4,'e')]
    |> Verify

(* Problem 1.11 *)
[<Scenario>]
let ``When RLE-encoding list [a,a,a,a,b,c,c,a,a,d,e,e,e,e] with encode_modified it should equal [[4,a],b,[2,c],[2,a],d,[4,e]]``() =
  Given ['a';'a';'a';'a';'b';'c';'c';'a';'a';'d';'e';'e';'e';'e']
    |> When calculating encode_modified
    |> It should equal [Tuple (4,'a'); Single 'b'; Tuple (2,'c'); Tuple (2,'a'); Single 'd'; Tuple (4,'e')]
    |> Verify

(** Problem 1.12 **)
[<Scenario>]
let ``When RLE-decoding list [[4,a],b,[2,c],[2,a],d,[4,e]] with decode it should equal [a,a,a,a,b,c,c,a,a,d,e,e,e,e]``() =
  Given [Tuple (4,'a'); Single 'b'; Tuple (2,'c'); Tuple (2,'a'); Single 'd'; Tuple (4,'e')]
    |> When calculating decode
    |> It should equal ['a';'a';'a';'a';'b';'c';'c';'a';'a';'d';'e';'e';'e';'e']
    |> Verify

(** Problem 1.13 **)
[<Scenario>]
let ``When RLE-encoding list [a,a,a,a,b,c,c,a,a,d,e,e,e,e] with encode_direct it should equal [[4,a],b,[2,c],[2,a],d,[4,e]]``() =
  Given ['a';'a';'a';'a';'b';'c';'c';'a';'a';'d';'e';'e';'e';'e']
    |> When calculating encode_direct
    |> It should equal [Tuple (4,'a'); Single 'b'; Tuple (2,'c'); Tuple (2,'a'); Single 'd'; Tuple (4,'e')]
    |> Verify

(* Problem 1.14 *)
[<Scenario>]
let ``When duplicating list [a,b,c,c,d] with dupli it should equal [a,a,b,b,c,c,c,c,d,d]``() =
  Given ['a';'b';'c';'c';'d']
    |> When calculating dupli
    |> It should equal ['a';'a';'b';'b';'c';'c';'c';'c';'d';'d']
    |> Verify

(** Problem 1.15 **)
[<Scenario>]
let ``When calcualting ndupli of list [a,b,c] and 3 it should equal [a,a,a,b,b,b,c,c,c]``() =
  Given ['a';'b';'c']
    |> When calculating ndupli 3
    |> It should equal ['a';'a';'a';'b';'b';'b';'c';'c';'c']
    |> Verify

(** Problem 1.16 **)
[<Scenario>]
let ``When dropping every third element from [a,b,c,d,e,f,g,h,i,k] using drop it should equal [a,b,d,e,g,h,k]``() =
  Given ['a';'b';'c';'d';'e';'f';'g';'h';'i';'k']
    |> When calculating drop 3
    |> It should equal ['a';'b';'d';'e';'g';'h';'k']
    |> Verify

(** Problem 1.17 **)
[<Scenario>]
let ``When splitting list [a,b,c,d,e,f,g,h,i,k] using split at position 3 it should equal [a,b,c],[d,e,f,g,h,i,k]``() =
  Given ['a';'b';'c';'d';'e';'f';'g';'h';'i';'k']
    |> When calculating split 3
    |> It should equal (['a';'b';'c'], ['d';'e';'f';'g';'h';'i';'k'])
    |> Verify

(** Problem 1.18 **)
[<Scenario>]
let ``When slicing list [a,b,c,d,e,f,g,h,i,k] using slice from 3 to 7 it should equal [c,d,e,f,g]``() =
  Given ['a';'b';'c';'d';'e';'f';'g';'h';'i';'k']
    |> When calculating slice 3 7
    |> It should equal ['c';'d';'e';'f';'g']
    |> Verify

(** Problem 1.19 **)
[<Scenario>]
let ``When rotating list [a,b,c,d,e,f,g,h] using rotate by 3 it should equal [d,e,f,g,h,a,b,c]``() =
  Given ['a';'b';'c';'d';'e';'f';'g';'h']
    |> When calculating rotate 3
    |> It should equal ['d';'e';'f';'g';'h';'a';'b';'c']
    |> Verify

[<Scenario>]
let ``When rotating list [a,b,c,d,e,f,g,h] using rotate by -2 it should equal [g,h,a,b,c,d,e,f]``() =
  Given ['a';'b';'c';'d';'e';'f';'g';'h']
    |> When calculating rotate -2
    |> It should equal ['g';'h';'a';'b';'c';'d';'e';'f']
    |> Verify

(* Problem 1.20 *)
[<Scenario>]
let ``When removing element at position 2 of list [a,b,c,d] using remove_at it should equal b,[a,c,d]``() =
  Given ['a';'b';'c';'d']
    |> When calculating remove_at 2
    |> It should equal ('b', ['a';'c';'d'])
    |> Verify

(* Problem 1.21 *)
[<Scenario>]
let ``When inserting element α at position 2 of list [a,b,c,d] using insert_at it should equal [a,α,b,c,d]``() =
  Given ['a';'b';'c';'d']
    |> When calculating insert_at 'α' 2
    |> It should equal ['a';'α';'b';'c';'d']
    |> Verify

(* Problem 1.22 *)
[<Scenario>]
let ``When creating a range from 4 to 9 using range it should equal [4,5,6,7,8,9]``() =
    When calculating range 4 9
    |> It should equal [4;5;6;7;8;9]
    |> Verify

(* Problem 1.23 *)


(* Problem 1.24 *)


(* Problem 1.25 *)


(** Problem 1.26 **)
[<Scenario>]
let ``When generating combinations of list [a,b,c,d] elements using combination of 2 it should equal [[a,b],[a,c],[a,d],[b,c],[b,d],[c,d]]``() =
  Given ['a';'b';'c';'d']
    |> When calculating combination 2
    |> It should equal [['a';'b'];['a';'c'];['a';'d'];['b';'c'];['b';'d'];['c';'d']]
    |> Verify

(** Problem 1.27a **)
[<Scenario>]
let ``When generating combinations of disjoint subsets of list [a,b,c,d,e,f,g,h,i] elements using group234 the combination count should equal 1260``() =
  Given ['a';'b';'c';'d';'e';'f';'g';'h';'i']
    |> When calculating group234 |> List.length
    |> It should equal 1260
    |> Verify

(** Problem 1.27b **)
[<Scenario>]
let ``When generating combinations of disjoint subsets of list [a,b,c,d,e] elements using combination of [1;2;1] the combination count should equal 60``() =
  Given ['a';'b';'c';'d';'e']
    |> When calculating group_comb [1;2;1] |> List.length
    |> It should equal 60
    |> Verify

(** Problem 1.28a **)
[<Scenario>]
let ``When sorting a list of lists [[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]] according to length of sublists with lsort it should equal [[o],[d,e],[d,e],[m,n],[a,b,c],[f,g,h],[i,j,k,l]]``() =
  Given [['a';'b';'c'];['d';'e'];['f';'g';'h'];['d';'e'];['i';'j';'k';'l'];['m';'n'];['o']]
    |> When calculating lsort
    |> It should equal [['o'];['d';'e'];['d';'e'];['m';'n'];['a';'b';'c'];['f';'g';'h'];['i';'j';'k';'l']]
    |> Verify

(** Problem 1.28b **)
[<Scenario>]
let ``When sorting a list of lists [[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]] according to their length frequency with lfsort it should equal [[i,j,k,l],[o],[a,b,c],[f,g,h],[d,e],[d,e],[m,n]]``() =
  Given [['a';'b';'c'];['d';'e'];['f';'g';'h'];['d';'e'];['i';'j';'k';'l'];['m';'n'];['o']]
    |> When calculating lfsort
    |> It should equal [['i';'j';'k';'l'];['o'];['a';'b';'c'];['f';'g';'h'];['d';'e'];['d';'e'];['m';'n']]
    |> Verify
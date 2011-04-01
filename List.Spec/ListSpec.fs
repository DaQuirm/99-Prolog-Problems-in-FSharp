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
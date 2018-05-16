(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1_1 = all_except_option ("string", ["string"]) = SOME []
val test1_2 = all_except_option ("string", ["toto", "string", "abc"]) = SOME ["toto", "abc"]
val test1_3 = all_except_option ("string", []) = NONE
val test1_4 = all_except_option ("string", ["cde"]) = NONE

val test2_1 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test_2_2 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred") = ["Fredrick", "Freddie", "F"]

val test3_1 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test_3_2 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred") = ["Fredrick", "Freddie", "F"]

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test7_1 = (remove_card ([(Hearts, Jack)], (Hearts, Ace), IllegalMove); false) handle IllegalMove => true

val test8_1 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test8_2 = all_same_color([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]) = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10_1 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test10_2 = score ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], 42) = 3

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)

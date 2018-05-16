(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test3 = longest_string2 ["A","bc", "de", "C"] = "de"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7_1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test7_2 = ((first_answer (fn x => if x > 7 then SOME x else NONE) [1,2,3,4,5]; false) handle NoAnswer => true)

val test8_1 = all_answers (fn x => if x > 1 then SOME [x -1] else NONE) [2,3,4,5,6,7] = SOME [1,2,3,4,5,6]
val test8_2 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a_1 = count_wildcards Wildcard = 1
val test9a_2 = count_wildcards (ConstructorP("toto", (TupleP [Wildcard, ConstructorP("tutu", Wildcard)]))) = 2
val test9a_3 = count_wildcards UnitP = 0

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10_1 = check_pat (Variable("x")) = true
val test10_2 = check_pat (ConstructorP("toto", (TupleP [Wildcard, ConstructorP("tutu", Variable("toto"))]))) = true
val test10_3 = check_pat (ConstructorP ("egg",ConstructorP ("egg",ConstP 4))) = true


val test11_1 = match (Const(1), UnitP) = NONE
val test11_2 = match (Const(1), ConstP(1)) = SOME []
val test11_3 = match (Constructor("toto", Const(5)), Wildcard) = SOME []
val test11_4 = match (Constructor("toto", Const(5)), ConstructorP("tutu", ConstP(5))) = NONE
val test11_5 = match (Constructor("toto", Const(5)), ConstructorP("toto", Variable("jojo"))) = SOME [("jojo", Const(5))]
val test11_6 = match (
    Constructor("toto", Tuple([Const(1), Const(4), Constructor("val", Const(2)), Constructor("bob", Tuple([Const(6), Const(9)]))])),
    ConstructorP("toto", TupleP([Wildcard, ConstP(4), Variable("tar"), ConstructorP("bob", Variable("zan"))]))) = SOME [
        ("tar", Constructor("val", Const(2))),
        ("zan", Tuple([Const(6), Const(9)]))
    ]

val test12_1 = first_match Unit [ConstructorP("toto", UnitP), UnitP] = SOME []

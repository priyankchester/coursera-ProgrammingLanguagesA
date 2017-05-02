(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test01 = only_capitals ["A","B","C"] = ["A","B","C"]
val test02 = only_capitals ["A","b","C"] = ["A","C"]
val test03 = only_capitals [] = []
val test04 = only_capitals ["a","b","c"] = []

val test20 = longest_string1 ["a","bc","c"] = "bc"
val test21 = longest_string1 ["ab","bc","cd"] = "ab"
val test22 = longest_string1 ["a","bc","def"] = "def"
val test23 = longest_string1 ["abc","de","f"] = "abc"
val test24 = longest_string1 ["","",""] = ""
val test25 = longest_string1 [] = ""

val test36 = longest_string2 ["A","bc","C"] = "bc"
val test30 = longest_string2 ["a","bc","c"] = "bc"
val test31 = longest_string2 ["ab","cd","ef"] = "ef"
val test32 = longest_string2 ["a","bc","def"] = "def"
val test33 = longest_string2 ["abc","de","f"] = "abc"
val test34 = longest_string2 ["","",""] = ""
val test35 = longest_string2 [] = ""

val test4a0 = longest_string3 ["A","bc","C"] = "bc"
val test4a1 = longest_string3 ["a","bc","c"] = "bc"
val test4a2 = longest_string3 ["ab","bc","cd"] = "ab"
val test4a3 = longest_string3 ["a","bc","def"] = "def"
val test4a4 = longest_string3 ["abc","de","f"] = "abc"
val test4a5 = longest_string3 ["","",""] = ""
val test4a6 = longest_string3 [] = ""

val test4b0 = longest_string4 ["A","B","C"] = "C"
val test4b1 = longest_string4 ["A","bc","C"] = "bc"
val test4b2 = longest_string4 ["a","bc","c"] = "bc"
val test4b3 = longest_string4 ["ab","cd","ef"] = "ef"
val test4b4 = longest_string4 ["a","bc","def"] = "def"
val test4b5 = longest_string4 ["abc","de","f"] = "abc"
val test4b6 = longest_string4 ["","",""] = ""
val test4b7 = longest_string4 [] = ""

val test50 = longest_capitalized ["A","bc","C"] = "A"
val test51 = longest_capitalized ["A","Bcd","C"] = "Bcd"
val test52 = longest_capitalized ["a","bcd","c"] = ""
val test54 = longest_capitalized ["AB","CD","EF"] = "AB"

val test60 = rev_string "" = ""
val test61 = rev_string "a" = "a"
val test62 = rev_string "ab" = "ba"
val test63 = rev_string "abc" = "cba"

val test70 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test71 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3]
handle NoAnswer => 0
val test71 = test71=0

val test80 = all_answers (fn x => if x > 3 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test81 = all_answers (fn x => if x > 3 then SOME [x] else NONE) [4,5,6,7]     = SOME([4,5,6,7])

val test90a = count_wildcards Wildcard = 1
val test91a = count_wildcards (Variable("s")) = 0
val test92a = count_wildcards (TupleP([Wildcard, Wildcard, UnitP])) = 2
val test93a = count_wildcards (ConstructorP("varName", Wildcard)) = 1

val test90b = count_wild_and_variable_lengths (Variable("a")) = 1
val test91b = count_wild_and_variable_lengths (Variable("ab")) = 2
val test92b = count_wild_and_variable_lengths (TupleP([Wildcard,Variable("abc")])) = 4
val test93b = count_wild_and_variable_lengths
(ConstructorP("varName",TupleP([Wildcard,Variable("abc"),Wildcard]))) = 5

val test90c = count_some_var ("x", Variable("x")) = 1
val test91c = count_some_var ("x", Wildcard) = 0
val test92c = count_some_var ("x", TupleP([Variable("x"), Wildcard])) = 1
val test93c = count_some_var ("x", TupleP([Variable("x"), Wildcard,Variable("x")])) = 2
val test94c = count_some_var ("x", TupleP([Variable("x"), Wildcard,Variable("xy")])) = 1

val test100a = extract_variable_names(Variable("s")) = ["s"]
val test101a = extract_variable_names(TupleP([Variable("s"),Variable("p")])) = ["s","p"]
val test102a = extract_variable_names(Wildcard) = []
val test104a = extract_variable_names(TupleP([Wildcard])) = []
val test105a = extract_variable_names(TupleP([Wildcard, Variable("s"), 
                                              Wildcard, Variable("abc")])) = ["s","abc"]

val test100b = contains_only_unique_values(["a","b","c"])                                              
val test101b = contains_only_unique_values(["a","b","a"])=false
val test102b = contains_only_unique_values(["a","b","c","d"])                                              

val test100 = check_pat (Variable("x")) = true
val test101 = check_pat (Wildcard) = true
val test102 = check_pat (TupleP([Wildcard,Variable("x")])) = true
val test103 = check_pat (TupleP([Wildcard,Variable("x"), Variable("x")])) = false
val test104 = check_pat (TupleP([Wildcard,Variable("x"), Variable("y")]))

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []

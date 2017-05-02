(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test10 = all_except_option ("string", ["string"]) = SOME []
val test11 = all_except_option("a",["a","b","c"]) = SOME(["b","c"]);
val test12 = all_except_option("a",["b","c","d"]) = NONE;
val test13 = all_except_option("a",["b","c","d","a"]) = SOME(["b","c","d"]);
val test14 = all_except_option("a",[]) = NONE;
val test15 = all_except_option("a",["a"]) = SOME([]);

val test20 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test21 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred")=["Fredrick","Freddie","F"]

val test22 = get_substitutions1
([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff")=["Jeffrey","Geoff","Jeffrey"]

val test30 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test31 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred")=["Fredrick","Freddie","F"]

val test32 = get_substitutions2
([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff")=["Jeffrey","Geoff","Jeffrey"]

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]
val test41 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"})

val test50 = card_color (Clubs, Num 2) = Black
val test51 = card_color (Hearts, Queen) = Red
val test52 = card_color (Diamonds, Jack) = Red
val test53 = card_color (Spades, King) = Black
val test54 = card_color (Clubs, Num 7) = Black
val test55 = card_color (Clubs, Num 9) = Black

val test60 = card_value (Clubs, Num 2) = 2
val test61 = card_value (Clubs, Ace) = 11
val test62 = card_value (Spades, Num 9) = 9
val test63 = card_value (Clubs, King) = 10
val test64 = card_value (Hearts, Jack) = 10
val test65 = card_value (Diamonds, Queen) = 10

val test70 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test71 = remove_card ([(Hearts, Ace), (Spades, King)], (Hearts, Ace),IllegalMove) = [(Spades,King)]
val test72 = remove_card ([(Hearts, Ace), (Spades, King), (Hearts,Ace)], (Hearts, Ace), IllegalMove) = 
                         [(Spades,King),(Hearts, Ace)]
val test73 = remove_card ([(Hearts, Ace), (Hearts, King), (Hearts, Queen)], (Hearts, Jack), IllegalMove)
                         handle IllegalMove => []
val test73 = test73=[] 

val test80 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test81 = all_same_color [(Hearts, Ace), (Diamonds, Ace)] = true
val test82 = all_same_color [(Hearts, Ace), (Spades, Ace)] = false
val test83 = all_same_color [(Clubs, Ace), (Spades, Ace)] = true
val test84 = all_same_color [] = true
val test85 = all_same_color [(Hearts, Ace)] = true

val test90 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test91 = sum_cards [(Clubs, Num 2),(Clubs, Num 5)] = 7
val test92 = sum_cards [(Clubs, Num 1),(Clubs, Num 2),(Clubs, Num 3)] = 6
val test93 = sum_cards [(Clubs, King),(Clubs, Queen)] = 20
val test94 = sum_cards [(Clubs, King),(Clubs, Ace)] = 21

val test95 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test98 = score ([(Hearts, Num 2),(Hearts, Num 4)],10) = 2
val test96 = score ([(Hearts, Num 2),(Clubs, Num 9)],10) = 3
val test97 = score ([(Hearts, Num 2),(Hearts, Num 9)],10) = 1

val goal = 15;
val held_card = [(Hearts, Num 2), (Clibs, Num 4)];
val test111 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], goal) = 6
val test114 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[], 10) = score(held_cards, goal)

val test112 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test113 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             

(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* 1.a *)
fun all_except_option(str: string, inputList: string list) = 
  let 
    fun aux(auxInputList: string list, acc: string list, found: bool) = 
      case auxInputList of
           [] => if found then SOME(acc) else NONE
         | head :: tail => 
             if same_string(head,str)
             then aux([], acc@tail, true)
             else aux(tail, acc@[head], found)
  in
    aux(inputList, [], false)
  end

(* 1.b *)
fun get_substitutions1(listOfListOfNames: string list list, str: string) = 
    case listOfListOfNames of
         [] => []
       | listOfNames :: remainingLists => 
           case all_except_option(str, listOfNames) of
             NONE => get_substitutions1(remainingLists, str)
           | SOME(result) => result @ get_substitutions1(remainingLists, str)

(* 1.c *)
fun get_substitutions2(listOfListOfNames: string list list, str: string) = 
let
  fun aux(listOfNames: string list list, acc: string list) =
    case listOfNames of
         [] => acc
       | head::tail => 
           case all_except_option(str, head) of
                NONE => aux(tail, acc)
              | SOME(result) => aux(tail, acc@result)
in
  aux(listOfListOfNames, [])
end


(* 1.d *)
fun similar_names(listOfListOfNames: string list list, 
                  {first: string, middle: string, last: string}) = 
  let
    val listOfFirstNames = get_substitutions2(listOfListOfNames, first)
    fun createNames(listOfNames: string list) =
      case listOfNames of
           [] => []
         | head :: tail => {first=head, middle=middle, last=last} :: createNames(tail)

  in
    {first=first, middle=middle, last=last} :: createNames(listOfFirstNames)
  end




(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* 2.a *)  
fun card_color(c: card) =
  case c of 
       (Clubs,_)    => Black
     | (Spades,_)   => Black
     | (Diamonds,_) => Red
     | (Hearts,_)   => Red

(* 2.b *)  
fun card_value(c: card) = 
  case c of
       (_,Num(i)) => i
     | (_,Ace)    => 11
     | (_,_)      => 10

(* 2.c *)  
fun remove_card(cs: card list, c: card, e: exn) =
  let
    fun aux_remove_card(cs': card list, acc: card list, found: bool) = 
      case cs' of
           []           => if found then acc else raise e
         | head :: tail => if(head=c andalso not found)
                           then acc @ tail 
                           else aux_remove_card(tail, acc @ [head], found)

  in
    aux_remove_card(cs, [], false)
  end

(* 2.d *)  
fun all_same_color(cs: card list) = 
  let
    fun aux_all_same_color(cs': card list, previousColor: color, isSame: bool) =
      case cs' of
           []   => isSame 
         | head :: tail => card_color(head) = previousColor andalso 
                           aux_all_same_color(tail, previousColor, true)
  in
    case cs of
        []            => true (* Empty list *)
       | _ :: []      => true (* One element list *)
       | head :: tail => aux_all_same_color(tail, card_color(head), false) (*2 or more elments*)
  end

(* 2.e *)  
fun sum_cards(cs: card list) = 
  let 
    fun aux_sum_cards(cs': card list, sum: int) =
      case cs' of
           [] => sum
         | head:: tail => aux_sum_cards(tail, sum + card_value(head))
  in
    aux_sum_cards(cs, 0)
  end

(* 2.f *)  
fun score(cs: card list, goal: int) = 
  let
    val sum        = sum_cards(cs);
    val prel_score = if (sum > goal)
                     then 3 * (sum - goal)
                     else (goal - sum)
  in
    if    all_same_color(cs)
    then  prel_score div 2
    else  prel_score
  end

(* 2.g *)  
fun officiate(card_list: card list, moves_list: move list, goal: int) = 
  (* Check in final version what the variable name is for moves and moves list *)
  let
    fun aux_officiate(cs: card list, held_cards: card list, moves: move list) = 
      case (moves, cs) of
           ([], _)                                      => score(held_cards, goal) (* No more valid moves: Game over *)
         | (Discard(c) :: remaining_moves, _)           =>  aux_officiate(cs, remove_card(held_cards, c, IllegalMove), remaining_moves)
         | (Draw :: _, [])                              =>  score(held_cards, goal)  (* No more cards to draw: Game over*) 
         | (Draw :: remaining_moves, cs_head::cs_tail)  => if (score(cs_head::held_cards, goal) > goal)
                                                           then
                                                             score(cs_head::held_cards, goal)
                                                           else
                                                             aux_officiate(cs_tail, cs_head::held_cards, remaining_moves)
  in
    aux_officiate(card_list, [], moves_list) (* game starts with held cards being empty list *)
  end

(*
Write a function 𝚊𝚕𝚝𝚎𝚛𝚗𝚊𝚝𝚎 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 that takes a list of numbers and
adds them with alternating sign. For example 𝚊𝚕𝚝𝚎𝚛𝚗𝚊𝚝𝚎 [𝟷,𝟸,𝟹,𝟺] = 𝟷 - 𝟸 + 𝟹 - 𝟺
= -𝟸. *)

fun alternate(intList: int list) =
let
  fun aux(internalList: int list, positiveSign: bool, acc: int) = 
    case (internalList,positiveSign) of
         ([],_) => acc
       | (head :: tail, true)  => aux(tail, not positiveSign, acc+head)
       | (head :: tail, false) => aux(tail, not positiveSign, acc-head)

in
  aux(intList, true, 0)
end

(*
Write a function 𝚖𝚒𝚗_𝚖𝚊𝚡 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 * 𝚒𝚗𝚝 that takes a non-empty list of
         numbers, and returns a pair (𝚖𝚒𝚗, 𝚖𝚊𝚡) of the minimum and maximum of
         the numbers in the list. 
         Assumption: input list is not empty
*)
fun min_max(intList: int list) =
let

  fun local_min_max(internalList: int list, min: int, max: int) =
    let
      fun maxi(val1: int, val2: int) = if(val1>val2) then val1 else val2
      fun mini(val1: int, val2: int) = if(val1<val2) then val1 else val2
    in
      case internalList of
         []         => (min, max)
       | head::tail => local_min_max(tail, mini(head,min), maxi(head,max))
    end
in
  local_min_max(tl intList, hd intList, hd intList)
end

(* Write a function 𝚌𝚞𝚖𝚜𝚞𝚖 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 that takes a list of numbers and returns a list of the partial sums of those numbers. For example 𝚌𝚞𝚖𝚜𝚞𝚖 [𝟷,𝟺,𝟸𝟶] = [𝟷,𝟻,𝟸𝟻]. *)
fun cumsum(intList: int list) = 
let
  fun aux(internalList: int list, acc: int) = 
    case internalList of
         []         => []
       | head::tail => head+acc :: aux(tail, head+acc)
in
  aux(intList, 0)
end

(* Write a function 𝚐𝚛𝚎𝚎𝚝𝚒𝚗𝚐 : 𝚜𝚝𝚛𝚒𝚗𝚐 𝚘𝚙𝚝𝚒𝚘𝚗 -> 𝚜𝚝𝚛𝚒𝚗𝚐 that given a string option 𝚂𝙾𝙼𝙴 name 
* returns the string "𝙷𝚎𝚕𝚕𝚘 𝚝𝚑𝚎𝚛𝚎, ...!" where the dots would be replaced by name. 
* Note that the name is given as an option, so if it is 𝙽𝙾𝙽𝙴 then replace the dots with "𝚢𝚘𝚞".
*)
fun greeting(nameOpt: string option) = 
  case nameOpt of
       SOME(name) => "Hello there, " ^ name ^ "!"
     | NONE => "Hello there, you!"

(* Write a function 𝚛𝚎𝚙𝚎𝚊𝚝 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 * 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 that given a list of
         integers and another list of nonnegative integers, repeats the integers
        in the first list according to the numbers indicated by the second list. For
        example: 𝚛𝚎𝚙𝚎𝚊𝚝 ([𝟷,𝟸,𝟹], [𝟺,𝟶,𝟹]) = [𝟷,𝟷,𝟷,𝟷,𝟹,𝟹,𝟹].*)

fun repeat(list1: int list, list2: int list) = 
  case (list1, list2) of 
     (hd1::tl1, hd2::tl2) => 
         if(hd2 > 0)
         then hd1 :: repeat(hd1::tl1, (hd2-1)::tl2)
         else repeat(tl1, tl2)

    | (_,_) => []


(*
Write a function 𝚊𝚍𝚍𝙾𝚙𝚝 : 𝚒𝚗𝚝 𝚘𝚙𝚝𝚒𝚘𝚗 * 𝚒𝚗𝚝 𝚘𝚙𝚝𝚒𝚘𝚗 -> 𝚒𝚗𝚝 𝚘𝚙𝚝𝚒𝚘𝚗 that given two
"optional" integers, adds them if they are both present (returning 𝚂𝙾𝙼𝙴 of their
sum), or returns 𝙽𝙾𝙽𝙴 if at least one of the two arguments is 𝙽𝙾𝙽𝙴.*)

fun addOpt(intOpt1: int option, intOpt2: int option) = 
  case (intOpt1, intOpt2) of
       (SOME(int1), SOME(int2)) => SOME(int1+int2)
     | _ => NONE


(*
Write a function 𝚊𝚍𝚍𝙰𝚕𝚕𝙾𝚙𝚝 : 𝚒𝚗𝚝 𝚘𝚙𝚝𝚒𝚘𝚗 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚘𝚙𝚝𝚒𝚘𝚗 that given a list of
       "optional" integers, adds those integers that are there (i.e. adds all
       the 𝚂𝙾𝙼𝙴 𝚒). For example: 𝚊𝚍𝚍𝙾𝚙𝚝 ([𝚂𝙾𝙼𝙴 𝟷, 𝙽𝙾𝙽𝙴, 𝚂𝙾𝙼𝙴 𝟹]) = 𝚂𝙾𝙼𝙴 𝟺. If
       the list does not contain any 𝚂𝙾𝙼𝙴 𝚒s in it, i.e. they are all 𝙽𝙾𝙽𝙴 or
       the list is empty, the function should return 𝙽𝙾𝙽𝙴. *)
fun addAllOpt(optIntListInput: int option list) = 
let
  fun aux(optIntList: int option list, accOpt: int option) = 
    case (optIntList, accOpt) of 
         ([],_)                           => accOpt
       | (SOME(headVal)::tail, SOME(acc)) => aux(tail, SOME(headVal+acc)) 
       | (SOME(headVal)::tail, NONE)      => aux(tail, SOME(headVal))
       | (NONE::tail, SOME(acc))          => aux(tail, SOME(acc))
       | (NONE::tail, NONE)               => aux(tail, NONE)
in
  aux(optIntListInput, NONE)
end

(*Write a function 𝚊𝚗𝚢 : 𝚋𝚘𝚘𝚕 𝚕𝚒𝚜𝚝 -> 𝚋𝚘𝚘𝚕 that given a list of booleans returns
* 𝚝𝚛𝚞𝚎 if there is at least one of them that is 𝚝𝚛𝚞𝚎, otherwise returns 𝚏𝚊𝚕𝚜𝚎.
* (If the list is empty it should return 𝚏𝚊𝚕𝚜𝚎 because there is no 𝚝𝚛𝚞𝚎.)*)
fun any(listOfBool: bool list) = 
  case listOfBool of
       [] => false
     | head::tail => head orelse any(tail)

(*Write a function 𝚊𝚕𝚕 : 𝚋𝚘𝚘𝚕 𝚕𝚒𝚜𝚝 -> 𝚋𝚘𝚘𝚕 that given a list of booleans returns
* 𝚝𝚛𝚞𝚎 if all of them 𝚝𝚛𝚞𝚎, otherwise returns 𝚏𝚊𝚕𝚜𝚎. (If the list is empty it
* should return 𝚝𝚛𝚞𝚎 because there is no 𝚏𝚊𝚕𝚜𝚎.)*)
fun all(listOfBool: bool list) =
  case listOfBool of
       [] => true
     | head::tail => head andalso all(tail)

(*Write a function 𝚣𝚒𝚙 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 * 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 * 𝚒𝚗𝚝 list that given two lists of integers
* creates consecutive pairs, and stops when one of the lists is empty. For example: 𝚣𝚒𝚙 ([𝟷,𝟸,𝟹], [𝟺, 𝟼]) = [(𝟷,𝟺), (𝟸,𝟼)].
Challenge: Write a version 𝚣𝚒𝚙𝚁𝚎𝚌𝚢𝚌𝚕𝚎 of 𝚣𝚒𝚙, where when one list is empty it starts recycling from
its start until the other list completes. For example: 𝚣𝚒𝚙𝚁𝚎𝚌𝚢𝚌𝚕𝚎 ([𝟷,𝟸,𝟹], [𝟷, 𝟸, 𝟹, 𝟺, 𝟻, 𝟼, 𝟽]) = [(𝟷,𝟷), (𝟸,𝟸), (𝟹, 𝟹), (𝟷,𝟺), (𝟸,𝟻), (𝟹,𝟼), (𝟷,𝟽)].
Lesser challenge: Write a version 𝚣𝚒𝚙𝙾𝚙𝚝 of 𝚣𝚒𝚙 with return type (𝚒𝚗𝚝 * 𝚒𝚗𝚝)
𝚕𝚒𝚜𝚝 𝚘𝚙𝚝𝚒𝚘𝚗. This version should return 𝚂𝙾𝙼𝙴 of a list when the original lists
have the same length, and 𝙽𝙾𝙽𝙴 if they do not.*)

fun zip(list1: int list, list2: int list) =
  case(list1, list2) of
       (hd1::tl1, hd2::tl2) => (hd1, hd2)::zip(tl1, tl2)
     | _ => []


fun zipRecycle(list1: int list, list2: int list) =
  case(list1, list2) of
       (hd1::tl1, hd2::tl2) => (hd1, hd2)::zip(tl1, tl2)
     | _ => []

(*Write a function 𝚕𝚘𝚘𝚔𝚞𝚙 : (𝚜𝚝𝚛𝚒𝚗𝚐 * 𝚒𝚗𝚝) 𝚕𝚒𝚜𝚝 * 𝚜𝚝𝚛𝚒𝚗𝚐 -> 𝚒𝚗𝚝 𝚘𝚙𝚝𝚒𝚘𝚗 that
* takes a list of pairs (𝚜, 𝚒) and also a string 𝚜𝟸 to look up. It then goes
* through the list of pairs looking for the string 𝚜𝟸 in the first component. If
* it finds a match with corresponding number 𝚒, then it returns 𝚂𝙾𝙼𝙴 𝚒. If it
* does not, it returns 𝙽𝙾𝙽𝙴.*)

fun lookup(listOfPairs: (string*int) list, searchString: string) =
let
  fun internalLookup(pairs: (string*int) list) =
    case pairs of
         [] => NONE
       | (s,i)::tail => if(s=searchString) then SOME(i) else internalLookup(tail)
in
  internalLookup(listOfPairs)
end


(*Write a version 𝚜𝚙𝚕𝚒𝚝𝙰𝚝 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 * 𝚒𝚗𝚝 -> 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 * 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 of the
* previous function that takes an extra "threshold" parameter, and uses that
* instead of 0 as the separating point for the two resulting lists.*)

fun splitAt(listOfInts: int list, threshold: int) = 
let
  fun aux(ints: int list, positives: int list, negatives: int list) = 
    case ints of
      []                     => (positives, negatives)
    | headInt::remainingInts => if headInt > threshold
                                then aux(remainingInts, positives @ [headInt], negatives)
                                else aux(remainingInts, positives, negatives @ [headInt])
in
  aux(listOfInts, [], [])
end


(*Write a function 𝚜𝚙𝚕𝚒𝚝𝚞𝚙 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 * 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 that given a list
* of integers creates two lists of integers, one containing the non-negative
* entries, the other containing the negative entries. Relative order must be
* preserved: All non-negative entries must appear in the same order in which
* they were on the original list, and similarly for the negative entries.*)

fun splitup(listOfInts: int list) = splitAt(listOfInts, 0)

(*Write a function 𝚒𝚜𝚂𝚘𝚛𝚝𝚎𝚍 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚋𝚘𝚘𝚕𝚎𝚊𝚗 that given a list of integers
* determines whether the list is sorted in increasing order.*)

fun isSorted(intList: int list) = 
let
  fun aux(auxList: int list, prev: int) = 
    case auxList of
         [] => true
       | head::tail => prev<head andalso aux(tail, head)
in
  case intList of
       [] => true
     | head::tail => aux(tail, head)
end

(*Write a function 𝚒𝚜𝙰𝚗𝚢𝚂𝚘𝚛𝚝𝚎𝚍 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚋𝚘𝚘𝚕𝚎𝚊𝚗, that given a list of
       * integers determines whether the list is sorted in either increasing or
       * decreasing order.*)

fun isAnySorted(intList: int list) = 
let
  fun reverse(list1: int list) =
    case list1 of
      []     => []
    | hd::tl => reverse(tl) @ [hd]    
in
  isSorted(intList) orelse isSorted(reverse(intList))
end

(*Write a function 𝚜𝚘𝚛𝚝𝚎𝚍𝙼𝚎𝚛𝚐𝚎 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 * 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 that takes two
* lists of integers that are each sorted from smallest to largest, and merges
* them into one sorted list. For example: 𝚜𝚘𝚛𝚝𝚎𝚍𝙼𝚎𝚛𝚐𝚎 ([𝟷,𝟺,𝟽], [𝟻,𝟾,𝟿]) =
* [𝟷,𝟺,𝟻,𝟽,𝟾,𝟿].
*)

fun sortedMerge(inputList1: int list, inputList2: int list) =
let
  fun aux(list1: int list, list2: int list, acc: int list) = 
    case(list1, list2) of
       ([],[]) => []
     | (list1, []) => acc@list1
     | ([], list2) => acc@list2
     | ((hd1::tl1, hd2::tl2)) => if(hd1<hd2)
                                 then aux(tl1,      hd2::tl2, acc @ [hd1])
                                 else aux(hd1::tl1, tl2,      acc @ [hd2])
in
  aux(inputList1, inputList2, [])
end  


(*
Write a sorting function 𝚚𝚜𝚘𝚛𝚝 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 that works as follows: Takes the first element out, and uses it as the "threshold" for 𝚜𝚙𝚕𝚒𝚝𝙰𝚝. It then recursively sorts the two lists produced by 𝚜𝚙𝚕𝚒𝚝𝙰𝚝. Finally it brings the two lists together. (Don't forget that element you took out, it needs to get back in at some point). You could use 𝚜𝚘𝚛𝚝𝚎𝚍𝙼𝚎𝚛𝚐𝚎 for the "bring together" part, but you do not need to as all the numbers in one list are less than all the numbers in the other.)
Write a function 𝚍𝚒𝚟𝚒𝚍𝚎 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 * 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 that takes a list of integers and produces two lists by alternating elements between the two lists. For example: 𝚍𝚒𝚟𝚒𝚍𝚎 ([𝟷,𝟸,𝟹,𝟺,𝟻,𝟼,𝟽]) = ([𝟷,𝟹,𝟻,𝟽], [𝟸,𝟺,𝟼]).
Write another sorting function 𝚗𝚘𝚝_𝚜𝚘_𝚚𝚞𝚒𝚌𝚔_𝚜𝚘𝚛𝚝 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 that works as follows: Given the initial list of integers, splits it in two lists using divide, then recursively sorts those two lists, then merges them together with 𝚜𝚘𝚛𝚝𝚎𝚍𝙼𝚎𝚛𝚐𝚎.
Write a function 𝚏𝚞𝚕𝚕𝙳𝚒𝚟𝚒𝚍𝚎 : 𝚒𝚗𝚝 * 𝚒𝚗𝚝 -> 𝚒𝚗𝚝 * 𝚒𝚗𝚝 that given two numbers 𝚔 and 𝚗 it attempts to evenly divide 𝚔 into 𝚗 as many times as possible, and returns a pair (𝚍, 𝚗𝟸) where 𝚍 is the number of times while 𝚗𝟸 is the resulting 𝚗 after all those divisions. Examples: 𝚏𝚞𝚕𝚕𝙳𝚒𝚟𝚒𝚍𝚎 (𝟸, 𝟺𝟶) = (𝟹, 𝟻) because 𝟸*𝟸*𝟸*𝟻 = 𝟺𝟶 and 𝚏𝚞𝚕𝚕𝙳𝚒𝚟𝚒𝚍𝚎((𝟹,𝟷𝟶)) = (𝟶, 𝟷𝟶)  because 𝟹 does not divide 𝟷𝟶.
Using 𝚏𝚞𝚕𝚕𝙳𝚒𝚟𝚒𝚍𝚎, write a function 𝚏𝚊𝚌𝚝𝚘𝚛𝚒𝚣𝚎 : 𝚒𝚗𝚝 -> (𝚒𝚗𝚝 * 𝚒𝚗𝚝) 𝚕𝚒𝚜𝚝 that given a number 𝚗 returns a list of pairs (𝚍, 𝚔) where 𝚍 is a prime number dividing 𝚗 and 𝚔 is the number of times it fits. The pairs should be in increasing order of prime factor, and the process should stop when the divisor considered surpasses the square root of 𝚗. If you make sure to use the reduced number 𝚗𝟸 given by 𝚏𝚞𝚕𝚕𝙳𝚒𝚟𝚒𝚍𝚎 for each next step, you should not need to test if the divisors are prime: If a number divides into 𝚗, it must be prime (if it had prime factors, they would have been earlier prime factors of 𝚗 and thus reduced earlier). Examples: 𝚏𝚊𝚌𝚝𝚘𝚛𝚒𝚣𝚎(𝟸𝟶) = [(𝟸,𝟸), (𝟻,𝟷)]; 𝚏𝚊𝚌𝚝𝚘𝚛𝚒𝚣𝚎(𝟹𝟼) = [(𝟸,𝟸), (𝟹,𝟸)]; 𝚏𝚊𝚌𝚝𝚘𝚛𝚒𝚣𝚎(𝟷) = [].
Write a function 𝚖𝚞𝚕𝚝𝚒𝚙𝚕𝚢 : (𝚒𝚗𝚝 * 𝚒𝚗𝚝) 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 that given a factorization of a number 𝚗 as described in the previous problem computes back the number 𝚗. So this should do the opposite of 𝚏𝚊𝚌𝚝𝚘𝚛𝚒𝚣𝚎.
Challenge (hard): Write a function 𝚊𝚕𝚕_𝚙𝚛𝚘𝚍𝚞𝚌𝚝𝚜 : (𝚒𝚗𝚝 * 𝚒𝚗𝚝) 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 that given a factorization list result from 𝚏𝚊𝚌𝚝𝚘𝚛𝚒𝚣𝚎 creates a list all of possible products produced from using some or all of those prime factors no more than the number of times they are available. This should end up being a list of all the divisors of the number 𝚗 that gave rise to the list. Example: 𝚊𝚕𝚕_𝚙𝚛𝚘𝚍𝚞𝚌𝚝𝚜([(𝟸,𝟸), (𝟻,𝟷)]) = [𝟷,𝟸,𝟺,𝟻,𝟷𝟶,𝟸𝟶]. For extra challenge, your recursive process should return the numbers in this order, as opposed to sorting them afterwards.
*)

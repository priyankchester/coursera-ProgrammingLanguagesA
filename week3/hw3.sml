(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1. Write a function only_capitals that takes a string list and returns a
* string list that has only
* the strings in the argument that start with an uppercase letter.  *)

val only_capitals = List.filter(fn s => Char.isUpper(String.sub(s,0)))

(* 2. Write a function longest_string1 that takes a string list and returns the
* longest string in the
* list. *)

fun longest_string1(xs: string list) = 
  foldl (fn(x, init) => if String.size init >= String.size x then init else x) "" xs

(* 3. Write a function longest_string2 that is exactly like longest_string1 except
*  in the case of ties
*  it returns the string closest to the end of the list. *)

fun longest_string2(xs: string list) = 
  foldl (fn(x, init) => if String.size x >= String.size init then x else init) "" xs

(* 4. Write functions longest_string_helper, longest_string3, and longest_string4  *)
val longest_string_helper = fn(f: int*int->bool) => fn(xs) =>
  foldl (fn(x,init) => if f(String.size x, String.size init) then x else init) "" xs

val longest_string3 = longest_string_helper(fn(a,b) => a>b)
val longest_string4 = longest_string_helper(fn(a,b) => a>=b)

(* 5. Write a function longest_capitalized that takes a string list and
* returns the longest string in
* the list that begins with an uppercase letter, or "" if there are no such
* strings.*)

val longest_capitalized = longest_string1 o only_capitals

(* 6. Write a function rev_string that takes a string and returns the string
* that is the same characters in
  * reverse order. *)
val rev_string = (implode o rev o explode)

(* Write a function first_answer of type (’a -> ’b option) -> ’a list -> ’b
* (notice the 2 arguments are curried). 
* The first argument should be applied to elements of the second
* argument in order until the first time it returns SOME v for 
* some v and then v is the result of the call to first_answer.
* If the first argument returns NONE for all list elements, then
* first_answer should raise the exception NoAnswer.  *)

(* 7. *)
val first_answer = fn(f) => fn(xs) => 
  case List.mapPartial f xs of
       []     => raise NoAnswer
     | x::xs' => x

(* 8. *)
val all_answers = fn(f) => fn(xs) =>
  let
    fun aux(answers, acc) =
        case answers of
           []           => SOME(acc) 
         | SOME(x)::xs' => aux(xs', acc@x)
         | NONE::_    => NONE 
  in
    aux(List.map f xs, [])
  end

(* Last part *)

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(* 9a. Use g to define a function count_wildcards that takes a pattern and
* returns how many Wildcard patterns it contains.  *)

val return_one = fn()  => 1
val return_zero = fn(s) => 0
val count_wildcards = g return_one return_zero

(* 9b. Use g to define a function count_wild_and_variable_lengths that takes a
* pattern and returns
* the number of Wildcard patterns it contains plus the sum of the string lengths
* of all the variables in the variable patterns it contains *)

val string_length = fn(s) => String.size(s)
val count_wild_and_variable_lengths = g return_one string_length

(* 9c. Use g to define a function count_some_var that takes a string and a
* pattern (as a pair) and
* returns the number of times the string appears as a variable in the pattern.  *)
fun count_some_var(variableName, p) =
  let
    val string_match = fn(s) => if s=variableName then 1 else 0
  in
    g return_zero string_match p
  end

(* 10. Write a function check_pat that takes a pattern and returns true if and
* only if all the variables
* appearing in the pattern are distinct from each other (i.e., use different
* strings). The constructor names are not relevant. *)
fun extract_variable_names(p) = 
  case p of
       Variable x        => [x]
     | TupleP ps         => List.foldl (fn(p,acc) => acc @ extract_variable_names(p)) [] ps
     | ConstructorP(_,p) => extract_variable_names(p)
     | _                 => []

fun contains_only_unique_values(variables) = 
  case variables of
       []     => true
     | x::xs' => not (List.exists(fn(a) => a=x) xs') andalso 
                 contains_only_unique_values(xs')

val check_pat = contains_only_unique_values o extract_variable_names

(* 11. Write a function match that takes a valu * pattern and returns a (string * valu) list option,
* namely NONE if the pattern does not match and SOME lst where lst is the list of bindings if it does. *)

fun match(v, p) =
  case(v,p) of
       (_, Wildcard)                           => SOME([])
     | (Unit,UnitP)                            => SOME([])
     | (Const(_),ConstP(_))                    => SOME([])
     | (_, Variable(s))                        => SOME([(s,v)])
     | (Constructor(s2,v), ConstructorP(s1,p)) => if s1=s2
                                                  then match(v,p)
                                                  else NONE
     | (Tuple(vs::vs'), TupleP(ps::ps'))       => if List.length vs' = List.length ps'
                                                  then
                                                    case (match(vs,ps),match(vs,ps)) of
                                                        (SOME(list1),SOME(list2)) => SOME(list1@list2)
                                                       | _                        => NONE
                                                  else NONE
     | (Tuple([]), TupleP([]))                 => SOME([])
     | _                                       => NONE


(* 12. Write a function first_match that takes a value and a list of patterns
* and returns a
* (string * valu) list option, namely NONE if no pattern in the list matches or
* SOME lst where
* lst is the list of bindings for the first pattern in the list that matches *)

fun first_match v ps = 
  SOME(first_answer (fn(p) => match(v,p)) ps)
  handle NoAnswer => NONE 


(*  1.
 * Write a function is_older that takes two dates and evaluates to true or false.
 * It evaluates to true if
 * the first argument is a date that comes before the second argument. (If the
 * two dates are the same,
 * the result is false.)
 *)
fun is_older(date1: int*int*int, date2: int*int*int) =

  let
    val year1  = #1 date1;
    val year2  = #1 date2;
    val month1 = #2 date1;
    val month2 = #2 date2;
    val day1   = #3 date1;
    val day2   = #3 date2;
  in
    year1 < year2 orelse 
    (year1=year2 andalso month1 < month2) orelse
    (year1=year2 andalso month1=month2 andalso day1 < day2)
  end

 (* 2.
  * Write a function number_in_month that takes a list of dates and a
  * month (i.e., an int) and returns
  * how many dates in the list are in the given month.
  *)
fun number_in_month(listOfDates:(int*int*int) list, month: int) =
  let
    fun number_in_month_helper(listOfDatesInner: (int*int*int) list, count:int) = 
      if(null listOfDatesInner)
      then  count
      else
        let
          val currDate = hd listOfDatesInner
          val currMonth = #2 currDate
        in
          if currMonth = month
          then 
            number_in_month_helper(tl listOfDatesInner, count+1)
          else
            number_in_month_helper(tl listOfDatesInner, count)
        end
  in
    number_in_month_helper(listOfDates, 0)
  end
 
(* 3
 * Write a function number_in_months that takes a list of dates and a list of
 * months (i.e., an int list)
 * and returns the number of dates in the list of dates that are in any
 * of the months in the list of months.
 * Assume the list of months has no number repeated 
 *)  
fun number_in_months(listOfDates:(int*int*int) list, months: int list):int =
  if null months
  then  0
  else
    number_in_month(listOfDates, hd months) + 
    number_in_months(listOfDates, tl months)

(*  4. 
 *  Write a function dates_in_month that takes a list of dates and a month (i.e.,
 *  an int) and returns a
 *  list holding the dates from the argument list of dates that are in the month.
 *  The returned list should
 *  contain dates in the order they were originally given.
 *)

fun dates_in_month(listOfDates: (int*int*int) list, month: int): (int*int*int)
  list =
  if null listOfDates
  then  []
  else
    let
      val currMonth = #2 (hd listOfDates);
    in
      if (currMonth = month)
      then hd listOfDates:: dates_in_month(tl listOfDates, month)
      else dates_in_month(tl listOfDates, month)
    end

 (* 5.
  * Write a function dates_in_months that takes a list of dates and a list of
  * months (i.e., an int list)
  * and returns a list holding the dates from the argument list of
  * dates that are in any of the months in
  * the list of months. Assume the list of months has no
  * number repeated
  *)

fun dates_in_months(listOfDates: (int*int*int) list, months: int list): (int*int*int) list = 

  if null months
  then []
  else
    dates_in_month(listOfDates, hd months) @ dates_in_months(listOfDates, tl months)

 (* 6.
 * Write a function get_nth that takes a list of strings and an int n and
 * returns the n
 * th element of the
 * list where the head of the list is 1st. Do not worry about the case where the
 * list has too few elements:
 * your function may apply hd or tl to the empty list in this case, which is
 * okay.
 *)

fun get_nth(listOfStrings: string list, n: int): string = 
  if n-1=0
  then  hd listOfStrings
  else
    get_nth(tl listOfStrings, n-1)

 (* 7.
 * Write a function date_to_string that takes a date and returns a string of the
 * form January 20, 2013
 * (for example). Use the operator ^ for concatenating strings and the library
 * function Int.toString
 * for converting an int to a string. 
 *)

fun date_to_string(date: int*int*int): string = 
  let
    val listOfMonths=["January", "February", "March", "April", "May", "June",
       "July", "August", "September", "October", "November", "December"];
    val month = #2 date;
    val year = #1 date;
    val day = #3 date;
  in
    get_nth(listOfMonths, month) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)
  end

(* 8.
 * Write a function number_before_reaching_sum that takes an int called sum,
 * which you can assume  is positive, and an int list, which you can assume 
 * contains all positive numbers, and returns an int.
 * You should return an int n such that the first n elements of the list add to
 * less than sum, but the first
 * n + 1 elements of the list add to sum or more.
 *)

fun number_before_reaching_sum(sum: int, listOfInts: int list): int = 
  if hd listOfInts >= sum
  then 0
  else
    1 + number_before_reaching_sum(sum-hd listOfInts, tl listOfInts) 

 (* 9.
 * Write a function what_month that takes a day of year (i.e., an int between 1
 * and 365) and returns
 * what month that day is in (1 for January, 2 for February, etc.). Use a list
 * holding 12 integers and your
 * answer to the previous problem.
 *)
fun what_month(day: int): int = 
  1+number_before_reaching_sum(day, 
    [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30,31])

 (* 10.
 * Write a function month_range that takes two days of the year day1 and day2
 * and returns an int list
 * [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ...,
 * and mn is the month
 * of day day2. Note the result will have length day2 - day1 + 1 or length 0 if
 * day1>day2.
 *)

fun month_range(day1: int, day2: int): int list = 
  if (day1 <=day2)
  then
    what_month(day1) :: month_range(day1+1, day2)
  else
    []

(* 11.
* Write a function oldest that takes a list of dates and evaluates to an
* (int*int*int) option. It
* evaluates to NONE if the list has no dates and SOME d if the date d is the
* oldest date in the list.
*)

fun oldest(listOfDates: (int*int*int) list): (int*int*int) option =
  if null listOfDates
  then NONE
  else 
    let
      val head = hd listOfDates
      val oldestFromList = oldest(tl listOfDates)
    in
      if isSome(oldestFromList) andalso is_older(valOf oldestFromList, head)
      then
        oldestFromList
      else
        SOME(head)
    end

 (* 12.
 * Write functions number_in_months_challenge and dates_in_months_challenge
 * that are like your solutions to problems 3 and 5 except having a month in the
 * second argument multiple
 * times has no more effect than having it once
 *)

(* 13.
* Write a function reasonable_date that takes a date and determines if it
* describes a real date in the common era. A “real date” has a positive year
* (year 0 did not exist), a
* month between 1 and 12, and a day appropriate for the month. 
*)
fun nth(listOfInts: int list, n: int): int =
  if n-1=0
  then hd listOfInts
  else nth(tl listOfInts, n-1)

fun reasonable_date(date: int*int*int): bool =
let 
  val year  = #1 date;
  val month = #2 date;
  val day   = #3 date;
  val calendar     = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30,31]
  val leapCalendar = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30,31]

  fun isLeapYear(year: int): bool = 
     (year mod 4 = 0 andalso year mod 100 <> 0)
     orelse year mod 400 = 0 

  fun dayMax(): int = 
    if isLeapYear(year)
    then nth(leapCalendar, month)
    else nth(calendar    , month)
in
  year  > 0  andalso year < 2017 andalso
  month >=1  andalso month <=12 andalso
  day   >=1  andalso day <= dayMax()
end

(* P05. *)
fun reverse(listOfIntegers: int list) = 
  let 
    fun reverse_helper(list1: int list, list2: int list) =
      if null list2
      then list1
      else reverse_helper(hd list2 :: list1, tl list2)
  in
    reverse_helper([], listOfIntegers)
  end

fun reverse2(listOfIntegers: int list) = 
  if null listOfIntegers
  then []
  else reverse2(tl listOfIntegers) @ [hd listOfIntegers]

(* P14. *)
fun duplicate( listOfInts: int list) = 
  if null listOfInts
  then []
  else
    hd listOfInts :: hd listOfInts :: duplicate(tl listOfInts)

(* P15. *)
fun duplicateN(n: int, listOfInts: int list) = 
  if null listOfInts
  then []
  else
    let
      fun repeat(n: int, num:int) =
        if n=0
        then []
        else num :: repeat(n-1, num)
    in
      repeat(n, hd listOfInts) @ duplicateN(n, tl listOfInts)
    end

(* P16. *)
fun drop(n: int, listOfInts: int list) = 
    let
      fun drop_helper(current_n: int, listOfInts2: int list) =
        if null listOfInts2
        then []
        else  
          if current_n-1 <> 0
          then hd listOfInts2 :: drop_helper(current_n-1, tl listOfInts2)
          else drop_helper(n, tl listOfInts2) 
    in
      drop_helper(n, listOfInts)
    end


(* P17. *)
fun split(n: int, listOfInts: int list): (int list * int list) =
  let 
    fun split_helper(n1: int, list1: int list, list2: int list) =
      if n1=0
      then (list1, list2)
      else split_helper(n1-1, list1 @ [hd list2], tl list2)
  in
    split_helper(n, [], listOfInts)
  end

(* P18. *)
fun slice(low: int, high: int, listOfInts: int list) = 
  if null listOfInts orelse low=high
  then []
  else
    if low=0
    then hd listOfInts :: slice(low+1, high, tl listOfInts)
    else slice(low-1, high-1, tl listOfInts)

  (* P20. *)
fun removeAt(n: int, listOfInts: int list) = 
  if null listOfInts
  then []
  else
    if n=0
    then tl listOfInts
    else hd listOfInts :: removeAt(n-1, tl listOfInts)

 (* P21. *)
fun insertAt(toInsert: int, position: int, listOfInts: int list) = 
  if null listOfInts
  then [toInsert]
  else
    if position=0
    then toInsert :: listOfInts
    else
      hd listOfInts :: insertAt(toInsert, position-1, tl listOfInts)

 (* P22. *)
fun range(low: int, high: int) = 
  if low > high
  then []
  else 
    if low=high
    then [high]
    else low :: range(low+1, high)

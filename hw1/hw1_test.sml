val test1a = is_older ((1,2,3),(2,3,4)) = true
val test1b = is_older ((2012,2,8),(2011,3,31)) = false
val test1c = is_older ((2011,4,28),(2011,3,31)) = false
val test1d = is_older ((2011,3,31),(2011,4,28)) = true
val test1e = is_older ((1,1,1),(1,1,1)) = false

val test4a = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4b = dates_in_month ([(2012,2,28),(2013,12,1), (2012,2,21)],2) = [(2012,2,28), (2012,2,21)]
val test4c = dates_in_month ([],2) = []
val test4d = dates_in_month ([(2012,3,22)],2) = []
val test4e = dates_in_month ([(2013,12,1), (2012,2,21), (2012,2,28)],2) = [(2012,2,21), (2012,2,28)]

val test5a = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6a = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test6b = get_nth (["hi", "there", "how", "are", "you"], 1) = "hi" 

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8a = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test8b = number_before_reaching_sum (10, [2, 3, 4, 1, 5]) = 3
val test8c = number_before_reaching_sum (1, [2, 3, 4, 1, 5]) = 0

val test9 = what_month 70 = 3
val test9b = what_month 1 = 1

val test10a = month_range (31, 34) = [1,2,2,2]
val test10b = month_range (31, 20) = []

val test11a = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11b = oldest([(2011,3,31),(2012,2,28),(2011,4,28)]) = SOME (2011,3,31)

val test12a = number_in_months_challenge ([(2011, 2, 21), (2011, 2, 1)], [2, 3, 2]) = 2
val test12b = number_in_months_challenge ([(2011, 2, 21), (2011, 2, 1)], [1, 3, 3]) = 0
val test12c = number_in_months_challenge ([(2011, 1, 1),(2011, 2, 1), (2011, 3, 1)], [1, 1, 2]) = 2
val test12d = number_in_months_challenge ([(2011, 1, 1),(2011, 2, 1), (2011, 3, 1)], [1, 2, 3]) = 3


val test13a = reasonable_date (2011, 1, 31)  = true
val test13b = reasonable_date (1900, 2, 29)  = false
val test13c = reasonable_date (2000, 2, 29)  = true
val test13d = reasonable_date (1900, 2, 30)  = false
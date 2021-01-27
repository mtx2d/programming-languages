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

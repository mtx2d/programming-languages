val test4a = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4b = dates_in_month ([(2012,2,28),(2013,12,1), (2012,2,21)],2) = [(2012,2,28), (2012,2,21)]
val test4c = dates_in_month ([],2) = []
val test4d = dates_in_month ([(2012,3,22)],2) = []
val test4e = dates_in_month ([(2013,12,1), (2012,2,21), (2012,2,28)],2) = [(2012,2,21), (2012,2,28)]

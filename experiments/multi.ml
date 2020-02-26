let rec test a b =
	if a>b then (test a (b+1);test (a-1) b)
		else a
;;
let rec sigma f n x =
	if n=0 then 0
		else f x + sigma f (n-1) x
;;

let sommeCarre n =
	let carre x = x*x in
		sigma carre n n
;;

let rond f g x=
	g (f x)
;;

let rec newtonRoot x y eps =
	if y=0. then x
	else if ((y*.y<=x+.eps)&&(y*.y>=x-.eps))
		then y
		else let y' = newtonRoo (y-.1.) in
			y=(y' +. (x/.y'))/.2.
;;
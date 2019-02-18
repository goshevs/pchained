simdata 500 3

*** y* is all y's that appear in the model except the depvar
*** fix omit() wildcards
*** add scaleomit and scaleinclude options (with wildcards)


pchained s1_i s2_i ///
		(y2, include(y3 mean(s1_i) sum(s2_i)) omit(x* y1)) ///
		(y3 i.yx x1 i.yz, include(y* mean(s2_i))) ///
		(y4, include(s1_i mean(s2_i)) omit(y*)), ///
		i(id) t(time) ///
		mod(y2 = "pmm, knn(3)" y3 = "regress" y4 = "regress") ///
		scaleo(s1_i = "x1 x3" s2_i = "x* y1") ///
		scalei(s1_i = "x*" s2_i = "y*") ///
		scalecov(x1 i.x2 x3 y1) addsad(y2 y3) ///
		mio(add(1) chaindots rseed(123456))
		
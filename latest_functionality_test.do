simdata 500 3

*** y* is all y's that appear in the model except the depvar
*** fix omit() wildcards
*** add scaleomit and scaleinclude options (with wildcards)

gen x5_base = rnormal()
/*
pchained s1_i s2_i ///
		(y2, include(y3 mean(s1_i) sum(s2_i)) omit(x* y1)) ///
		(y3 i.yx x1 i.yz, include(y* mean(s2_i))) ///
		(y4, include(s1_i mean(s2_i)) omit(y*)), ///
		i(id) t(time) ///s
		mod(y2 = "pmm, knn(3)" y3 = "regress" y4 = "regress") ///
		scaleo(s1_i = "x1 x3" s2_i = "x* y1") ///
		scalei(s1_i = "x1 x3 x4" s2_i = "i.yz i.yx y2 y3 y4") ///
		comcov(i.x2 y1) ///
		mio(add(1) chaindots rseed(123456))
		
*/
		
		
pchained (s1_i x*, include(y3 sum(s2_i)) omit(x* y1) scale cont) ///
		 (s2_i x4 y1, include(y3 mean(s1_i)) omit(y1) scale) ///
		 (y2, include(y3 mean(s1_i) sum(s2_i)) omit(x* y1)) ///
		 (y3 i.yx x1 i.yz, include(y* mean(s2_i))) ///
		 (y4, include(s1_i mean(s2_i)) omit(y*)), ///
		 i(id) t(time) ///
		 mod(y2 = "pmm, knn(3)" y3 = "regress" y4 = "regress") ///
		 common(y1) ///
		 condc(s1_i = "if y1 > -1" y4 = "if x2 >= 0") ///
		 condi(s2_i = "if mean(s1_i) > 0" y2 = "if y4 > -1") ///
		 mio(add(1) chaindots rseed(123456))
		

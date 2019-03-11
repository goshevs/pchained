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

	 
simdata 500 3

bys id: gen x5_base = x5[1]

*************
*** >>>>>>> Ensuring the nesting condition holds
*** The following is IMPORTANT as otherwise Stata will throw an error
*** For details see README and the Stata Manual

foreach var of varlist s5_i* {
	replace `var' = -9999999 if x5_base < 0
}

egen mymiss = rowmiss(s1_i*)
egen mymean = rowmean(s1_i*)
replace mymean = . if mymiss > 0
		
foreach var of varlist s6_i* {
	replace `var' = -9999999 if mymean < 1
}

drop mymiss mymean

replace y5 = -9999999 if y4 < 0  // assign a large number that can be replaces with missing after imputation
replace y6 = -9999999 if x5 < 0  // assign a large number that can be replaces with missing after imputation
 
*** >>>>>>>
*************

pchained (s1_i, include(mean(s5_i s6_i) sum(s2_i)) scale omit(x*)) /// 
		 (s2_i, include(mean(s1_i) sum(s5_i s6_i)) scale omit(x5_base)) ///
		 (s5_i, include(mean(s1_i s2_i s6_i)) scale) ///
		 (s6_i, include(s1_i mean(s2_i s5_i)) scale omit(x5_base)) ///
		 (y2 i.yz, noimputed omit(x* y*))  ///
		 (y4 i.yx i.yz x5, include(y2 mean(s1_i)) omit(x5_base)) ///
		 (y5 i.yx x1 i.yz x5 i.x2, include(y*) omit(x5_base)) ///
		 (y6, noimputed omit(x* y*)), ///
	     i(id) t(time) ///
		 common(x1 i.x2 x3 y1 x5*) ///
		 mod(s1_i = "pmm, knn(3)" s2_i = "pmm, knn(3)" s5_i = "pmm, knn(3)" s6_i = "pmm, knn(3)" ///
		 	 y2 = "regress" y4 = "pmm, knn(3)" y5 = "pmm, knn(3)" y6 = "pmm, knn(3)") ///
		 condc(s5_i = "if x5_base > -1" y6 = "if x5 >= 0") ///
		 condi(s6_i = "if mean(s1_i) > 0" y5 = "if y4 > -1") ///
		 mio(add(1) chaindots rseed(123456) dryrun)
  
exit


pchained (s1_i x1, include(y3 sum(s2_i)) omit(y1) scale) ///
		 (s2_i x4 y1, include(y2 mean(s1_i)) omit(y1) scale cont) ///
		 (y2, include(y3 mean(s1_i) sum(s2_i)) omit(i.x2)) ///
		 (y3 i.yx x1 i.yz, include(y* mean(s2_i))) ///
		 (y4, include(s1_i mean(s2_i)) omit(y*)), ///
		 i(id) t(time) ///
		 mod(y2 = "pmm, knn(3)" y3 = "regress" y4 = "regress") ///
		 common(y1 i.x2) ///
		 condc(s1_i = "if y1 > -1" y4 = "if x2 >= 0") ///
		 condi(s2_i = "if mean(s1_i) > 0" y2 = "if y4 > -1") ///
		 mio(add(1) chaindots rseed(123456))
		

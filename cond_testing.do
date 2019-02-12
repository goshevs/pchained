

********************************************************************************
*** Test of conditionals for scales


simdata 500 3

bys id: gen x5_base = x5[1]

*************
*** The following is IMPORTANT as otherwise Stata will throw an error
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
*************


pchained s1_i s2_i s5_i s6_i ///
			  (y2 i.x2, noimputed)  ///
			  (y4 i.yx x1 i.yz x5, include(y2 mean(s1_i))) ///
			  (y5 i.yx x1 i.yz x5 i.x2, include(y2 y4)) ///
			  (y6 i.yx i.x2, noimputed), ///
	          i(id) t(time) scalecov(x1 i.x2 x3 y1 x5_base) mio(add(1) chaindots rseed(123456)) ///
			  mod(s1_i = "pmm, knn(3)" s2_i = "pmm, knn(3)" s5_i = "pmm, knn(3)" s6_i = "pmm, knn(3)" ///
				  y2 = "regress" y4 = "pmm, knn(3)" y5 = "pmm, knn(3)" y6 = "pmm, knn(3)") ///
			  condc(s5_i = "if x5_base > -1") ///
			  condi(s6_i = "if mean(s1_i) > 0") // print suspend
			  
			  
			  
exit
			  
*
********************************************************************************
*** Test of conditionals for stand-alone variables

simdata 500 3

*** The following is IMPORTANT as otherwise Stata will throw the following error:
/*
conditional(): no complete observations outside conditional sample;
    imputation variable contains only missing values outside the conditional sample.  This is not
    allowed.  The imputation variable must contain at least one nonmissing value outside the
    conditional sample.

r(459)	
*/
replace y5 = -9999999 if y4 < 0  // assign a large number that can be replaces with missing after imputation


/* Stata will throw a bunch of errors either about non-positive VCE or missing imputed values produced if the following is
not done
*/
replace y6 = -9999999 if x5 < 0  // assign a large number that can be replaces with missing after imputation
 
pchained s1_i s2_i (y2 x2, noimputed) (y4 i.yx x1 i.yz x5, include(y2 mean(s1_i))) (y5 i.yx x1 i.yz x5, include(y2 y4)) (y6 x2, noimputed), ///
	          i(id) t(time) scalecov(x1 i.x2 x3 y1 x5) mio(add(1) chaindots rseed(123456)) ///
			  mod(s1_i = "pmm, knn(3)" y2 = "regress" y4 = "pmm, knn(3)" y5 = "pmm, knn(3)" y6 = "pmm, knn(3)") ///
			  condi(y5 = "if y4 > -1") ///
			  condc(y6 = "if x5 >= 0")
			  
exit
			  
			  
			  
simdata 500 3
pchained s1_i s3_i, i(id) t(time) full scalecov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456))

			  
			  
			  
/*			  
pchained s1_i s2_i (y2 x2, noimputed) (y3 i.yx x1 i.yz, include(y2 mean(s1_i))), ///
	          i(id) t(time) scalecov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456)) ///
			  mod(s1_i = "pmm, knn(3)" y2 = "regress" y3 = "regress") ///
			  condi(s1_i = "if mean(s2_i) < sum(s2_i) | y2 > 3" y2 = "if y3 < 20" y3 = "if y2 ~= mean(s1_i) | y2 < sum(s2_i)") ///
			  condc(s1_i = "if x1 < 10" y3 = "if x2 < 1") print suspend
*/

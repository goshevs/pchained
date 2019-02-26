* Examples of Plumpton with -mi impute chained-
* Developers: Simo Goshev, Zitong Liu
*
*
*
*
*
********************************************************************************
*** Examples of running Plumpton with mi impute chained
********************************************************************************

clear
set more off

*******************
***  One scale  ***

*** Categorical items
simdata 500 3
pchained s1_i, i(id) t(time) comcov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456)) 

*** Treat items as continuous
simdata 200 3
pchained s1_i, i(id) t(time) cont(s1_i) comcov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456))

*** Items continuous by design (imputation model defined by user)
simdata 200 3
pchained s4_i, i(id) t(time) comcov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456)) /// 
               mod(s4_i = "pmm, knn(3)")


*******************
*** Two scales  ***

*** Categorical items
simdata 500 3
pchained s1_i s3_i, i(id) t(time) comcov(x1 i.x2 x3 y1) score(sum) mio(add(1) chaindots rseed(123456)) print suspend


*** Treat some scales as continuous
simdata 500 3
pchained s1_i s2_i, i(id) t(time) cont(s2_i) comcov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456))

*** Some scales/items continuous by design (imputation models defined by user)
simdata 500 3
pchained s2_i s4_i, i(id) t(time) comcov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456)) ///
                    mod(s2_i = "ologit" s4_i = "pmm, knn(3)")


********************
*** Three scales ***

*** Categorical items
simdata 500 3
pchained s1_i s2_i s3_i, i(id) t(time) comcov(x1 i.x2 x3 y1) score(sum) mio(add(1) chaindots rseed(123456))


*** Treat some scales as continuous
simdata 500 3
pchained s1_i s2_i s3_i, i(id) t(time) cont(s2_i) comcov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456))


*** Some scales/items continuous by design
simdata 500 3
pchained s1_i s3_i s4_i, i(id) t(time) comcov(x1 i.x2 x3 y1) mio(add(1) chaindots)


*** Mixed, s4_i by design is cont, s2_i user defined as cont
simdata 500 3
pchained s1_i s2_i s4_i, i(id) t(time) cont(s2_i) comcov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456))


********************
***   By group   ***

simdata 1000 3
pchained s1_i s4_i, i(id) t(time) comcov(x1 i.x2 x3 y1) score(sum) mio(add(1) chaindots by(group) rseed(123456))


*************************
***  Sampling Weight  ***

simdata 1000 3
pchained s1_i s4_i [pw=weight], i(id) t(time) comcov(x1 i.x2 x3 y1) score(sum) mio(add(1) chaindots rseed(123456))


****************************************************************
***  Imputing non-scale variables together with scale items  ***

*** 
simdata 500 3
pchained s1_i (y2, noimputed) (y3 i.yx x1 i.yz, include(y2 mean(s1_i))), ///
	          i(id) t(time) comcov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456)) ///
			  mod(s1_i = "pmm, knn(3)" y2 = "regress" y3 = "regress")


*** 		  
simdata 500 3
pchained s1_i (y2, include(y3 mean(s1_i)) omit(x1 i.x2)) (y3 i.yx x1 i.yz, include(y2 mean(s1_i))), ///
	          i(id) t(time) comcov(x1 i.x2 x3 y1) addsad(y2 y3) mio(add(1) chaindots rseed(123456)) ///
			  mod(y2 = "pmm, knn(3)" y3 = "regress")


simdata 500 3
pchained s1_i s2_i (y2, include(y3 mean(s1_i) sum(s2_i)) omit(x1 i.x2 y1)) (y3 i.yx x1 i.yz, include(y2 mean(s2_i))), ///
	          i(id) t(time) comcov(x1 i.x2 x3 y1) addsad(y2 y3) mio(add(1) chaindots rseed(123456)) ///
			  mod(y2 = "pmm, knn(3)" y3 = "regress")

			  

*******************************************
***  Imputing non-scale variables only  ***

simdata 500 3
pchained (y2, include(y3)) ///
		 (y3 i.yx x1 i.yz, include(y2)), ///
		 comcov(x1 i.x2 x3 y1) ///
	     i(id) t(time) mio(add(1) chaindots rseed(123456)) ///
		 mod(y2 = "pmm, knn(3)" y3 = "regress")



		 
******************************************
***  Imputation subject to conditions  ***	 
		 
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
	
pchained s1_i s2_i s5_i s6_i ///
			  (y2 x5*, noimputed)  ///
			  (y4 i.yx i.yz x5, include(y2 mean(s1_i))) ///
			  (y5 i.yx x1 i.yz x5 i.x2, include(y*)) ///
			  (y6, noimputed omit(x*)), ///
	          i(id) t(time) comcov(x1 i.x2 x3 y1 x5*) mio(add(1) chaindots rseed(123456)) ///
			  mod(s1_i = "pmm, knn(3)" s2_i = "pmm, knn(3)" s5_i = "pmm, knn(3)" s6_i = "pmm, knn(3)" ///
				  y2 = "regress" y4 = "pmm, knn(3)" y5 = "pmm, knn(3)" y6 = "pmm, knn(3)") ///
			  condc(s5_i = "if x5_base > -1" y6 = "if x5 >= 0") ///
			  condi(s6_i = "if mean(s1_i) > 0" y5 = "if y4 > -1")
			  
	  


********************************************************************************
***  Imputing on complete remaining scales (Plumpton bypass)                 ***

***		
simdata 500 3
pchained s1_i s3_i, i(id) t(time) full comcov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456))

	  
	  
*** Generate aggregates off of imputed vars
*mi xeq: egen s1_sum = rowtotal(s1*)
*mi xeq: egen s1_mean = rowmean(s1*)


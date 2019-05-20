* Examples of usage of -pchained-
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

********************************************************************************
***  One scale  ***

*** Categorical items, no covariates
simdata 200 3
preserve
pchained (s1_i, noimputed scale), ///
          i(id) t(time) ///
          mio(add(1) chaindots rseed(123456) dryrun)
	  
*** Categorical items plus covariates
simdata 200 3
pchained (s1_i, noimputed scale), ///
          i(id) t(time) ///
          common(x1 i.x2 x3 y1) ///
          mio(add(1) chaindots rseed(123456) dryrun)

*** Categorical items as continuous
simdata 200 3
pchained (s1_i, noimputed scale cont), ///
          i(id) t(time) ///
          common(x1 i.x2 x3 y1) ///
          mio(add(1) chaindots rseed(123456) dryrun)

*** Items continuous by design
simdata 200 3
pchained (s4_i, noimputed scale), ///
          i(id) t(time) ///
          common(x1 i.x2 x3 y1) ///
          mio(add(1) chaindots rseed(123456) dryrun) 

*** Items continuous by design (imputation model defined by user)
simdata 200 3
pchained (s4_i, noimputed scale), ///
          i(id) t(time) ///
          common(x1 i.x2 x3 y1) ///
          mod(s4_i = "pmm, knn(3)") ///
          mio(add(1) chaindots rseed(123456) dryrun) 


		    
********************************************************************************
*** Two scales  ***

*** Categorical items
**** In s1_i include the sum of s3_i
**** In s3_i include the mean of s1_i
simdata 500 3
pchained (s1_i, noimputed include(sum(s3_i)) scale) ///
         (s3_i, noimputed include(mean(s1_i)) scale), ///
          i(id) t(time) ///
          mio(add(1) chaindots rseed(123456) dryrun)
		 

*** Scale s2_i as continuous
simdata 500 3
pchained (s1_i, noimputed include(mean(s2_i)) scale) ///
         (s2_i, noimputed include(mean(s1_i)) scale cont), ///
          i(id) t(time) ///
          common(x1 i.x2 x3 y1) ///
          mio(add(1) chaindots rseed(123456) dryrun)
		 

*** Include model-specific regressors and omit some common covariates
simdata 500 3
pchained (s2_i, noimputed include(mean(s4_i)) scale) ///
         (s4_i x5, noimputed include(sum(s2_i)) scale omit(x*)), ///
          i(id) t(time) ///
          common(x1 i.x2 x3 y1) ///
          mio(add(1) chaindots rseed(123456) dryrun)
		 


********************************************************************************
*** Three scales ***

*** Categorical items
simdata 500 3
pchained (s1_i, noimputed include(sum(s2_i s3_i)) scale) ///
         (s2_i, noimputed include(sum(s1_i s3_i)) scale) ///
         (s3_i, noimputed include(sum(s1_i s2_i)) scale), ///
          i(id) t(time) ///
          common(x1 i.x2 x3 y1) ///
          mio(add(1) chaindots rseed(123456) dryrun)
		 
*** Assign s1_i as continuous, add model-specific covariates, different scale scores
simdata 500 3
pchained (s1_i, noimputed include(sum(s2_i s3_i)) scale cont) ///
         (s2_i x4 x5, noimputed include(sum(s1_i) mean(s3_i)) scale) ///
         (s3_i x5, noimputed include(sum(s1_i s2_i)) scale), ///
          i(id) t(time) ///
          common(x1 i.x2 x3 y1) ///
          mio(add(1) chaindots rseed(123456) dryrun)



********************************************************************************
***   By group   ***

simdata 1000 3
pchained (s1_i, noimputed include(sum(s4_i)) scale) ///
         (s4_i, noimputed include(mean(s1_i)) scale), ///
          i(id) t(time) ///
          common(x1 i.x2 x3 y1) ///
          mio(add(1) chaindots  by(group) rseed(123456) dryrun)


		 
********************************************************************************
*** Weighted imputation ***

simdata 1000 3
pchained (s1_i, noimputed include(sum(s4_i)) scale) ///
         (s4_i, noimputed include(mean(s1_i)) scale) ///
         [pw=weight], i(id) t(time) ///
          common(x1 i.x2 x3 y1) ///
          mio(add(1) chaindots rseed(123456) dryrun)



********************************************************************************
***  Imputing non-scale variables together with scale items  ***

***
simdata 500 3
pchained (s1_i, noimputed scale) ///
         (y2, noimputed omit(x* y*)) ///
         (y3 i.yx i.yz, include(y2 mean(s1_i))), ///
          i(id) t(time) ///
          common(x1 i.x2 x3 y1) ///
          mod(s1_i = "pmm, knn(3)" y2 = "regress" y3 = "regress") ///
          mio(add(1) chaindots rseed(123456) dryrun)
		 
*** 		 
simdata 500 3
pchained (s1_i, noimputed include(y2 y3) scale) ///
         (y2, noimputed include(y* mean(s1_i)) omit(x* y*)) ///
         (y3 i.yx i.yz, include(y2 sum(s1_i))), ///
          i(id) t(time) ///
          common(x1 i.x2 x3 y1) ///
          mod(s1_i = "pmm, knn(3)" y2 = "regress" y3 = "regress") ///
          mio(add(1) chaindots rseed(123456) dryrun)

		 

********************************************************************************
***  Imputing non-scale variables only  ***

***
simdata 500 3
pchained (y2, include(y3)) ///
         (y3 i.yx x1 i.yz, include(y2)), ///
          i(id) t(time) ///
          common(x1 i.x2 x3 y1) ///
          mod(y2 = "pmm, knn(3)" y3 = "regress") ///
          mio(add(1) chaindots rseed(123456) dryrun)

		 
		 
********************************************************************************
***  Imputation subject to conditions  ***	 

***
simdata 500 3

bys id: gen x5_base = x5[1]

*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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

// assign a large negative number that can be replaced with missing after imputation
replace y5 = -9999999 if y4 < 0
replace y6 = -9999999 if x5 < 0
 
*** >>>>>>>
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#d ;
pchained (s1_i, include(mean(s5_i s6_i) sum(s2_i)) scale omit(x*)) 
         (s2_i, include(mean(s1_i) sum(s5_i s6_i)) scale omit(x5_base))
         (s5_i, include(mean(s1_i s2_i s6_i)) scale)
         (s6_i, include(y* s1_i mean(s2_i s5_i)) scale omit(x5_base))
         (y2 i.yz, noimputed omit(x* y*))
         (y4 i.yx i.yz x5, include(y2 mean(s1_i)) omit(x5_base))
         (y5 i.yx x1 i.yz x5 i.x2, include(y*) omit(x5_base))
         (y6, noimputed omit(x* y*)),
          i(id) t(time)
          common(x1 i.x2 x3 y1 x5*)
          mod(s1_i = "pmm, knn(3)" s2_i = "pmm, knn(3)" 
              s5_i = "pmm, knn(3)" s6_i = "pmm, knn(3)"
              y2 = "regress" y4 = "pmm, knn(3)" 
              y5 = "pmm, knn(3)" y6 = "pmm, knn(3)")
          condc(s5_i = "if x5_base > -1" y6 = "if x5 >= 0") 
          condi(s6_i = "if mean(s1_i) > 0" y5 = "if y4 > -1") 
          mio(add(1) chaindots rseed(123456) dryrun);
#d cr



********************************************************************************
***  Dv's with wild cards; model with conditions ***	 

***
simdata 500 3

bys id: gen x5_base = x5[1]

*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*** >>>>>>> Ensuring the nesting conditions hold

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

// assign a large negative number that can be replaced with missing after imputation
replace y5 = -9999999 if y4 < 0
replace y6 = -9999999 if x5 < 0

replace ys1 = -9999999 if x6 < 0
replace ys2 = -9999999 if x6 < 0

*** >>>>>>>
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

pchained (s1_i i.x2, include(mean(s5_i s6_i) sum(s2_i)) scale omit(x*)) /// 
         (s2_i, include(mean(s1_i) sum(s5_i s6_i)) scale omit(x5_base)) ///
         (s5_i x3, include(mean(s1_i s2_i s6_i)) scale omit(x* y*)) ///
         (s6_i, include(s1_i mean(s2_i s5_i)) scale omit(x5_base)) ///
         (y4 i.yx i.yz x5* i.x2, include(y*) omit(x*)) ///
         (y5, noimputed omit(x* y*)) /// 
         (y6, noimputed omit(x* y*)) /// 
         (ys* i.yz i.yx x5*, noimputed include(y*)), ///
          i(id) t(time) ///
          common(x1 i.x2 x3 y1 x5* x6) ///
          mod(s1_i = "pmm, knn(3)" s2_i = "pmm, knn(3)" ///
              s5_i = "pmm, knn(3)" s6_i = "pmm, knn(3)" ///
              ys* = "pmm, knn(3)" y4 = "regress" ///
              y5 = "regress" y6 = "pmm, knn(3)") ///
          condc(s5_i = "if x5_base > -1" y6 = "if x5 >= 0" ys* = "if x6 > 0" ) ///
          condi(s6_i = "if mean(s1_i) > 0" y5 = "if y4 > -1") ///
          mio(add(1) chaindots rseed(123456) dryrun)



********************************************************************************
***  Dv's with wild cards; model with conditions; categorical predictors ***	 

***
simdata 500 3

bys id: gen x5_base = x5[1]
		 
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*** >>>>>>> Ensuring the nesting conditions hold

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

// assign a large negative number that can be replaced with missing after imputation
replace y5 = -9999999 if y4 < 0
replace y6 = -9999999 if x5 < 0

replace ys1 =  -9999999 if x6 < 0
replace ys2 =  -9999999 if x6 < 0

*** >>>>>>>
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

pchained (s1_i i.x2, include(mean(s5_i s6_i) sum(s2_i)) scale omit(x*) catself) /// 
         (s2_i, include(i.s1_i sum(s5_i s6_i)) scale omit(x5_base)) ///
         (s5_i x3, noimputed scale omit(x* y*)) ///
         (s6_i, include(mean(s2_i s5_i)) scale omit(x5_base)) ///
         (y4 i.yx i.yz x5* i.x2, include(i.y*) omit(x*)) ///
         (y5, noimputed omit(x* y*)) /// 
         (y6, noimputed omit(x* y*)) /// 
         (ys* i.yz i.yx x5*, noimputed include(i.y*) catself),   ///
          i(id) t(time) ///
          common(x1 i.x2 x3 y1 x5* x6) ///
          mod(s1_i = "pmm, knn(3)" s2_i = "pmm, knn(3)" ///
              s5_i = "pmm, knn(3)" s6_i = "pmm, knn(3)" ///
              ys* = "pmm, knn(3)" y4 = "regress" ///
              y5 = "regress" y6 = "pmm, knn(3)") ///
          condc(s5_i = "if x5_base > -1" y6 = "if x5 >= 0" ys* = "if x6 > 0" ) ///
          condi(s6_i = "if mean(s1_i) > 0" y5 = "if y4 > -1") ///
          mio(add(1) chaindots rseed(123456) dryrun)



********************************************************************************
***  Imputing on complete scales (Plumpton bypass) ***

***		
simdata 500 3
pchained (s1_i, noimputed include(s3_i) scale) ///
         (s3_i, noimputed include(s1_i) scale), ///
          i(id) t(time) ///
          common(x1 i.x2 x3 y1) ///
          mio(add(1) chaindots rseed(123456) dryrun)


	  
*** Generate aggregates off of imputed vars
*mi xeq: egen s1_sum = rowtotal(s1*)
*mi xeq: egen s1_mean = rowmean(s1*)


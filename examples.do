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
pchained s1_i, p(id) t(time) cov(x1 i.x2 x3 y) mio(add(1) chaindots rseed(123456))

*** Treat items as continuous
simdata 200 3
pchained s1_i, p(id) t(time) cont(s1_i) cov(x1 i.x2 x3 y) mio(add(1) chaindots rseed(123456))

*** Items continuous by design (imputation model defined by user)
simdata 200 3
pchained s4_i, p(id) t(time) cov(x1 i.x2 x3 y) mio(add(1) chaindots rseed(123456)) mod(s4_i = "pmm, knn(3)")


*******************
*** Two scales  ***

*** Categorical items
simdata 200 3
pchained s1_i s3_i, p(id) t(time) cov(x1 i.x2 x3 y) score("sum") mio(add(1) chaindots rseed(123456))


*** Treat some scales as continuous
simdata 500 3
pchained s1_i s2_i, p(id) t(time) cont(s2_i) cov(x1 i.x2 x3 y) mio(add(1) chaindots rseed(123456))

*** Some scales/items continuous by design (imputation models defined by user)
simdata 200 3
pchained s2_i s4_i, p(id) t(time) cov(x1 i.x2 x3 y) mio(add(1) chaindots rseed(123456)) mod(s2_i = "ologit" s4_i = "pmm, knn(3)")


********************
*** Three scales ***

*** Categorical items
simdata 200 3
pchained s1_i s2_i s3_i, p(id) t(time) cov(x1 i.x2 x3 y) score(mean) mio(add(1) chaindots rseed(123456))


*** Treat some scales as continuous
simdata 200 3
pchained s1_i s2_i s3_i, p(id) t(time) cont(s2_i) cov(x1 i.x2 x3 y) score(mean) mio(add(1) chaindots rseed(123456))


*** Some scales/items continuous by design
simdata 200 3
pchained s1_i s3_i s4_i, p(id) t(time) cov(x1 i.x2 x3 y) score(mean) mio(add(1) chaindots)


*** Mixed, s4_i by design is cont, s2_i user defined as cont
simdata 200 3
pchained s1_i s2_i s4_i, p(id) t(time) cont(s2_i) cov(x1 i.x2 x3 y) score(mean) mio(add(1) chaindots rseed(123456))


********************
***   By group   ***

simdata 1000 3
pchained s1_i s4_i, p(id) t(time) cov(x1 i.x2 x3 y) score(sum) mio(add(1) chaindots by(group) rseed(123456))


*************************
***  Sampling Weight  ***

simdata 500 3
pchained s1_i s4_i [pw=weight], p(id) t(time) cov(x1 i.x2 x3 y) score(sum) mio(add(1) chaindots rseed(123456))


*** Generate aggregates off of imputed vars
*mi xeq: egen s1_sum = rowtotal(s1*)
*mi xeq: egen s1_mean = rowmean(s1*)


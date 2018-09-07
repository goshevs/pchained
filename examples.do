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
simdata 200 3
pchained s1_i, p(id) t(time) cov(x1 i.x2 x3 y) mio("add(1) chaindots ")

*** Treat items as continuous
simdata 200 3
pchained s1_i, p(id) t(time) cont(s1_i) cov(x1 i.x2 x3 y) mio("add(1) chaindots ")

*** Items continuous by design
simdata 200 3
pchained s4_i, p(id) t(time) cov(x1 i.x2 x3 y) mio("add(1) chaindots ")


*******************
*** Two scales  ***

*** Categorical items
simdata 200 3
pchained s1_i s3_i, p(id) t(time) cov(x1 i.x2 x3 y) score("sum") mio("add(1) chaindots ")


*** Treat some scales as continuous
simdata 200 3
pchained s1_i s2_i, p(id) t(time) cont(s2_i) cov(x1 i.x2 x3 y) mio("add(1) chaindots ")

*** Some scales/items continuous by design
simdata 200 3
pchained s2_i s4_i, p(id) t(time) cov(x1 i.x2 x3 y) mio("add(1) chaindots ")




********************
*** Three scales ***

*** Categorical items
simdata 200 3
pchained s1_i s2_i s3_i, p(id) t(time) cov(x1 i.x2 x3 y) score("mean") mio("add(1) chaindots ")


*** Treat some scales as continuous
simdata 200 3
pchained s1_i s2_i s3_i, p(id) t(time) cont(s2_i) cov(x1 i.x2 x3 y) score("mean") mio("add(1) chaindots")


*** Some scales/items continuous by design
simdata 200 3
pchained s1_i s3_i s4_i, p(id) t(time) cov(x1 i.x2 x3 y) score("mean") mio("add(1) chaindots ")


*** Mixed, s4_i by design is cont, s2_i user defined as cont
simdata 200 3
pchained s1_i s2_i s4_i, p(id) t(time) cont(s2_i) cov(x1 i.x2 x3 y) score("mean") mio("add(1) chaindots ")



********************
***   By group   ***

simdata 1000 3
pchained s1_i s4_i, p(id) t(time) cov(x1 i.x2 x3 y) score("sum") mio("add(1) chaindots by(group)")


*** Generate aggregates off of imputed vars
*mi xeq: egen s1_sum = rowtotal(s1*)
*mi xeq: egen s1_mean = rowmean(s1*)


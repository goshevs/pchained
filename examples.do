* Examples of Plumpton with -mi impute chained-
* Author: Simo Goshev
* Date: 7/11/2018
* Version: 0.1
*
*
*
********************************************************************************
*** Examples of running Plumpton with mi impute chained
********************************************************************************
set more off
*** Add ado files to adopath
* adopath + "/Users/zitongliu/Dropbox/2018/BTProject/simdata"

*** Examples
*** One scale

simdata
pchained s1_i, p(id) t(time) cov(x1 i.x2 x3 y) mio("add(1) chaindots")

*** Two scales
simdata
pchained s1_i s2_i, p(id) t(time) cov(x1 i.x2 x3 y) score("sum") mio("add(1) chaindots ")


*** Three scales
simdata
pchained s1_i s2_i s3_i, p(id) t(time) cov(x1 i.x2 x3 y) score("mean") mio("add(1) chaindots ")



exit


********************************************************************************
*** Some additional commands that may come handy
********************************************************************************

** Short syntax 1
** pchained s1_i s2_i, panelvar(id) timevar(time)

** Short syntax 2
** pchained s1_i s2_i, p(id) t(time) cov(x1 i.x2 x3 y) score("mean") mio("add(1)")

** Full syntax
** pchained s1_i s2_i, panelvar(id) timevar(time) covars(x1 i.x2 x3 y) scoretype("mean") mioptions("add(1)")


*** Generate aggregates off of imputed vars
*mi xeq: egen s1_sum = rowtotal(s1*)
*mi xeq: egen s1_mean = rowmean(s1*)


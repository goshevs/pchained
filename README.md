Stata command -pchained-
===

*Developers*: Simo Goshev, Zitong Liu   
*Maintainer*: Simo Goshev  
*Group*: BC Research Services


Introduction
---

This is a new Stata command which implements Plumpton-type 
chained imputation of scales using -mi impute chained- (Plumpton, 2016).


Syntax
---

```
pchained namelist [if], Panelvar(varlist) Timevar(varname)
					   [CONTinous(namelist) SCOREtype(string)
					    COVars(varlist fv) MIOptions(string) 
					    SAVEmidata(string) CATCutoff(integer)
					    MINCsize(integer)]
```

Required inputs:


| input       | description            |
|-------------|------------------------|
| *namelist*  | unique stub names of the scale(s) to be imputed (takes multiple scales) |
| *Panelvar*  | cluster identifier (i.e. person, firm, country id) |
| *Timevar*   | time/wave identifier |


Options available to the user:


| option      | description            |
|-------------|------------------------|
| CONTinous   | stub names of scales whose items should be treated as continuous |
| SCOREtype   | mean score (default) or sum score |
| COVars      | list of covariates, supports factor variable syntax  |
| MIOptions   | mi impute chained options to be passed on (by() is also allowed) |
| SAVEmidata  | save the mi data; valid path and filename required|
| CATCutoff   | max number of categories/levels to classify as categorical; if fails --> classified as continuous |
|             | default: 10 |
| MINCsize    | minium cell size required for item to be included in analysis; if fails --> classified as rare |
|             | default: 0 |





Examples
---

```
*******************
***  One scale  ***

*** Categorical items
simdata 200 3
pchained s1_i, p(id) t(time) cov(x1 i.x2 x3 y)  ///
         mio("add(1) chaindots rseed(123456)")

*** Treat items as continuous
simdata 200 3
pchained s1_i, p(id) t(time) cont(s1_i) cov(x1 i.x2 x3 y) ///
         mio("add(1) chaindots rseed(123456) ")

*** Items continuous by design
simdata 200 3
pchained s4_i, p(id) t(time) cov(x1 i.x2 x3 y) /// 
         mio("add(1) chaindots rseed(123456)")


*******************
*** Two scales  ***

*** Categorical items
simdata 200 3
pchained s1_i s3_i, p(id) t(time) cov(x1 i.x2 x3 y) score("sum") ///
         mio("add(1) chaindots rseed(123456)")


*** Treat some scales as continuous
simdata 500 3
pchained s1_i s2_i, p(id) t(time) cont(s2_i) cov(x1 i.x2 x3 y) /// 
         mio("add(1) chaindots rseed(123456)")

*** Some scales/items continuous by design
simdata 200 3
pchained s2_i s4_i, p(id) t(time) cov(x1 i.x2 x3 y) ///
         mio("add(1) chaindots rseed(123456)")




********************
*** Three scales ***

*** Categorical items
simdata 200 3
pchained s1_i s2_i s3_i, p(id) t(time) cov(x1 i.x2 x3 y) score("mean") ///
         mio("add(1) chaindots rseed(123456)")


*** Treat some scales as continuous
simdata 200 3
pchained s1_i s2_i s3_i, p(id) t(time) cont(s2_i) cov(x1 i.x2 x3 y) /// 
         score("mean") mio("add(1) chaindots rseed(123456)")


*** Some scales/items continuous by design
simdata 200 3
pchained s1_i s3_i s4_i, p(id) t(time) cov(x1 i.x2 x3 y) score("mean") /// 
         mio("add(1) chaindots")


*** Mixed, s4_i by design is cont, s2_i user defined as cont
simdata 200 3
pchained s1_i s2_i s4_i, p(id) t(time) cont(s2_i) cov(x1 i.x2 x3 y) ///
         score("mean") mio("add(1) chaindots rseed(123456)")



********************
***   By group   ***

simdata 1000 3
pchained s1_i s4_i, p(id) t(time) cov(x1 i.x2 x3 y) score("sum") ///
         mio("add(1) chaindots by(group) rseed(123456)")
```










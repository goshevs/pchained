Stata command `pchained`
===

*Developers*: Simo Goshev, Zitong Liu   
*Maintainer*: Simo Goshev  
*Group*: BC Research Services


Introduction
---

This is a new Stata command which implements Plumpton-type 
chained imputation of scales using `mi impute chained` (Plumpton, 2016).


Installation
---

To load `pchained`, include the following line in your do file:

```
qui do "https://raw.githubusercontent.com/goshevs/pchained/master/pchained.ado"
```


Syntax
---

```
pchained scale_stubs [sadv_models] [if] [in] [weight], Ivar(varlist) Timevar(varname)
					   [CONTinous(namelist) SCOREtype(string asis) ///
						SCALECOVars(varlist fv) SCALEInclude(string asis) ///
						SCALEOmit(string asis) ADDSADepvars(varlist) /// 
						MIOptions(string asis) CATCutoff(integer 10) ///
						MINCsize(integer 0) MERGOptions(string asis) ///
						MODel(string asis) SAVEmidata(string) ///
						CONDImputed(string asis) CONDComplete(string asis) ///
						FULLscales PRINTmodel suspend debug] 
```
<br>

`pchained` takes the following arguments:

**Required**

| argument       | description            |
|----------------|------------------------|
| *scale_stubs*  | unique stub names of the scale(s) to be imputed (takes multiple scales); can be omitted if at least one `sadv_models` is defined (Sarah Jensen's discovery!) |
| *Ivar*         | unique cluster/panel identifier (i.e. person, firm, country id) |
| *Timevar*      | time/wave identifier |

<br>

**Optional and conditionally required arguments:**

| argument       | description            |
|----------------|------------------------|
| *sadv_models*  | models for stand-alone variables to be imputed; see below for specific syntax|
| *CONTinous*    | stub names of scales whose items should be treated as continuous |
| *SCOREtype*    | mean score or sum score |
|                | default: `mean` |
| *SCALECOVars*  | list of covariates to be included in the scale item imputatation models, supports factor variable syntax  |
| *SCALEInclude* |  TODO |
| *SCALEOmit*    |  TODO |
| *ADDSADepvars* | list of stand-alone variables to be included in the scale item imputation equations; all periods of these variables are used in the imputation equation for an item |
| *MIOptions*    | `mi impute chained` options to be passed on (`by()` is also allowed) |
| *CATCutoff*    | maximum number of categories/levels to classify as categorical; if higher --> classified as continuous |
|                | default: `10` |
| *MINCsize*     | minimum cell size required for item to be included in analysis; if lower --> classified as rare |
|                | default: `0` |
| *MERGOptions*  | merge options to be passed on to `merge` upon merging the imputed data with the original data; imputed dataset is *master*, original dataset is *using* |
|                | default: `keep(match)` |
| *MODel*        | the user can pass a model and options to `mi impute chained` for each imputed scale and stand-alone variable; this is a conditionally required argument; see below for details |
| *CONDImputed*  | conditional imputation; corresponds to `cond()` in `mi impute chained`; see below for syntax |
| *CONDComplete* | imputation conditional on the values of a complete regressor; see below for syntax |
| *FULLscales*   | bypasses the Plumpton imputation approach and instead imputes using the complete set of items from remaining scales |
| *SAVEmidata*   | save the mi data; valid path and filename required |
| *PRINTmodel*   | prints the imputation model |
| *suspend*      | turns the control of the imputation over to the user by suspending -pchained- immediately before imputation. Works only if `PRINTmodel` is specified |

<br>

`fweight`, `aweight`, `pweight` and `iweight` are allowed. However, different `mi impute chained` models may 
impose restrictions. Please, see the help file of `mi impute chained` for further guidance.

<br>

**Syntax for stand-alone variables**

`pchained` could be used for imputing scale items only or for scale items and stand-alone variables, 
dependently or independently of each other. A stand-alone variable model has the following general format:

`(depvar [covariateList][, options])`

<br>

Multiple `sadv_models` could be specified using the following syntax:

`(depvar1 [covariateList1][, options1])[(depvar2 [covariateList2][, options2]) ...]`

<br>

The arguments that a model in `sadv_models` takes are:

- `depvar`: the stand-alone variable to be imputed
- `covariateList`: an optional list of covariates to be included in the imputation equation of `depvar`. If 
`covariateList` is specified, `SCALECOVars` are excluded from the imputation model for `depvar`. 
- `options`: could be any set of:
    - `include([other_sadv] [mean(scale_stubs)] [sum(scale_stubs)])`: allows 
	the user to specify other stand-alone variables, `other_sadv`, as well as the types 
	of scale scores of the scales being imputed to be used as predictors in the 
	imputation model for `depvar`; if `include` is specified, option `noimputed` is assumed.
	If `other_sadv` is specified, all periods of the stand-alone variables in `other_sadv` are used as
	predictors in the imputation equation. If `mean` or `sum` is specified, the score for the
	time period corresponding to the time period of `depvar` is included as a regressor 
	- `omit(varlist)`: allows the user to remove covariates listed in `SCALECOVars` 
	from the imputation equation for `depvar`; this options is ignored if `include` is specified
	- `noimputed`: instructs Stata to remove all other imputed variables used as 
	predictors in the imputation equation, except other time periods of `depvar` and 
	variables specified in `include` 
	- other options specific to the imputation model

If `sadv_models` is specified, option `MODel` is required for all stand-alone variables. 

<br>

**Imputation subject to conditions**

`pchained` supports conditioning on complete regressors and imputed variables (i.e. conditional imputation). 
Conditioning on scale scores, means and sums, is also supported.

To condition on complete regressors, the user needs to specify option `CONDComplete`. 
The option has the following syntax:

`condc(scale_stub or depvar = "if condition" [scale_stub or depvar = "if condition"]...)` 

where `condition` is either a standard Stata condition or a condition which involves `means` or `sums` of scales. 
Multiple conditions for multiple scales/stand-alone varibles can be specified. See section Examples for usage.

To request conditional imputation, the user has to specify option `CONDImputed`. The option has the same syntax as 
`CONDComplete` but the variables that are being conditioned on have to be imputed variables.

An important requirement for imputation subject to conditions is to ensure "that missing 
values of all conditioning variables [are] nested within missing values 
of the conditional variable" (Stata Multiple-Imputation Reference Manual Release 15, p. 161).
This requirement, which we will refer to as "the nesting condition" may be unclear to novice 
users and therefore we have provided a use case in the Examples section.


<br>

Working with sensitive data?
---

If you are working with sensitive data, please ensure that you point Stata to a secure
directory that it can use as a temporary directory. Please, see [this](https://www.stata.com/support/faqs/data-management/statatmp-environment-variable/) reference for 
instructions on how to do this.

<br>

Examples
---

```
********************************************************************************
***  One scale                                                               ***

*** Categorical items
simdata 500 3
pchained s1_i, i(id) t(time) scalecov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456))

*** Treat items as continuous
simdata 200 3
pchained s1_i, i(id) t(time) cont(s1_i) scalecov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456))

*** Items continuous by design (imputation model defined by user)
simdata 200 3
pchained s4_i, i(id) t(time) scalecov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456)) /// 
               mod(s4_i = "pmm, knn(3)")


********************************************************************************
*** Two scales                                                               ***

*** Categorical items
simdata 500 3
pchained s1_i s3_i, i(id) t(time) scalecov(x1 i.x2 x3 y1) score(sum) mio(add(1) chaindots rseed(123456))


*** Treat some scales as continuous
simdata 500 3
pchained s1_i s2_i, i(id) t(time) cont(s2_i) scalecov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456))

*** Some scales/items continuous by design (imputation models defined by user)
simdata 500 3
pchained s2_i s4_i, i(id) t(time) scalecov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456)) ///
                    mod(s2_i = "ologit" s4_i = "pmm, knn(3)")


********************************************************************************
*** Three scales                                                             ***

*** Categorical items
simdata 500 3
pchained s1_i s2_i s3_i, i(id) t(time) scalecov(x1 i.x2 x3 y1) score(sum) mio(add(1) chaindots rseed(123456))


*** Treat some scales as continuous
simdata 500 3
pchained s1_i s2_i s3_i, i(id) t(time) cont(s2_i) scalecov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456))


*** Some scales/items continuous by design
simdata 500 3
pchained s1_i s3_i s4_i, i(id) t(time) scalecov(x1 i.x2 x3 y1) mio(add(1) chaindots)


*** Mixed, s4_i by design is cont, s2_i user defined as cont
simdata 500 3
pchained s1_i s2_i s4_i, i(id) t(time) cont(s2_i) scalecov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456))


********************************************************************************
***   By group                                                               ***

simdata 1000 3
pchained s1_i s4_i, i(id) t(time) scalecov(x1 i.x2 x3 y1) score(sum) mio(add(1) chaindots by(group) rseed(123456))


********************************************************************************
***  Sampling Weight                                                         ***

simdata 1000 3
pchained s1_i s4_i [pw=weight], i(id) t(time) scalecov(x1 i.x2 x3 y1) score(sum) mio(add(1) chaindots rseed(123456))


********************************************************************************
***  Imputing non-scale variables together with scale items                  ***

*** 
simdata 500 3
pchained s1_i (y2, noimputed) (y3 i.yx x1 i.yz, include(y2 mean(s1_i))), ///
	          i(id) t(time) scalecov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456)) ///
			  mod(s1_i = "pmm, knn(3)" y2 = "regress" y3 = "regress")

*** 		  
simdata 500 3
pchained s1_i (y2, include(y3 mean(s1_i)) omit(x1 i.x2)) (y3 i.yx x1 i.yz, include(y2 mean(s1_i))), ///
	          i(id) t(time) scalecov(x1 i.x2 x3 y1) addsad(y2 y3) mio(add(1) chaindots rseed(123456)) ///
			  mod(y2 = "pmm, knn(3)" y3 = "regress")

***
simdata 500 3
pchained s1_i s2_i (y2, include(y3 mean(s1_i) sum(s2_i)) omit(x1 i.x2 y1)) (y3 i.yx x1 i.yz, include(y2 mean(s2_i))), ///
	          i(id) t(time) scalecov(x1 i.x2 x3 y1) addsad(y2 y3) mio(add(1) chaindots rseed(123456)) ///
			  mod(y2 = "pmm, knn(3)" y3 = "regress")


			  

********************************************************************************
***  Imputing subject to conditions                                          ***


***
simdata 500 3
bys id: gen x5_base = x5[1]

*** ----->>>>>> Ensuring the nesting condition holds
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
*** <<<<<<<----

pchained s1_i s2_i s5_i s6_i ///
			  (y2 i.x2, noimputed)  ///
			  (y4 i.yx x1 i.yz x5, include(y2 mean(s1_i))) ///
			  (y5 i.yx x1 i.yz x5 i.x2, include(y2 y4)) ///
			  (y6 i.yx i.x2, noimputed), ///
			  i(id) t(time) ///
			  scalecov(x1 i.x2 x3 y1 x5_base) mio(add(1) chaindots rseed(123456)) ///
			  mod(s1_i = "pmm, knn(3)" s2_i = "pmm, knn(3)" s5_i = "pmm, knn(3)" s6_i = "pmm, knn(3)" ///
				  y2 = "regress" y4 = "pmm, knn(3)" y5 = "pmm, knn(3)" y6 = "pmm, knn(3)") ///
			  condc(s5_i = "if x5_base > -1") ///
			  condi(s6_i = "if mean(s1_i) > 0")

			  
*** 
simdata 500 3

*** ----->>>>>> Ensuring the nesting condition holds
replace y5 = -9999999 if y4 < 0  
replace y6 = -9999999 if x5 < 0  
*** <<<<<<<----

pchained s1_i s2_i ///
			  (y2 x2, noimputed) ///
			  (y4 i.yx x1 i.yz x5, include(y2 mean(s1_i))) ///
			  (y5 i.yx x1 i.yz x5, include(y2 y4)) (y6 x2, noimputed), ///
			  i(id) t(time) ///
			  scalecov(x1 i.x2 x3 y1 x5) mio(add(1) chaindots rseed(123456)) ///
			  mod(s1_i = "pmm, knn(3)" y2 = "regress" y4 = "pmm, knn(3)" ///
			      y5 = "pmm, knn(3)" y6 = "pmm, knn(3)") ///
			  condi(y5 = "if y4 > -1") ///
			  condc(y6 = "if x5 >= 0")	  
		
		
		
********************************************************************************
***  Imputing on complete remaining scales (Plumpton bypass)                 ***

***		
simdata 500 3
pchained s1_i s3_i, i(id) t(time) full scalecov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456))

```

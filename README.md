Stata command `pchained`
===

*Developers*: Simo Goshev, Zitong Liu   
*Maintainer*: Simo Goshev  
*Group*: BC Research Services


Introduction
---

This is a new Stata command which is a wrapper around `mi impute chained` and 
offers simplified syntax for imputation in longitudinal data. It also offers 
functinality for conducting scale imputation at the item level,
including Plumpton-type chained imputation of scales (Plumpton, 2016).


Installation
---

To load `pchained`, include the following line in your do file:

```
qui do "https://raw.githubusercontent.com/goshevs/pchained/master/pchained.ado"
```


What's new?
---

**Critical changes**

The most recent release, `pchained` v1.0, unifies model definition for
imputed variables, scales or stand-alone dependent variables, and simplifies 
syntax while providing a much greater degree of flexiblity in model specification.
 
The most recent version is **not backward compatable** and therefore old code
has to be updated to the new syntax to ensure proper operation.

**Added functionality**

The following new functionality was added to `pchained` in this version:

- Imputation subject to conditions. See options `CONDImputed` and `CONDComplete`
- Imputation on a full set of scale items rather than on functions of scale items. 
See examples below for an illustration.
- Wildcards (i.e. `*`) can now be used in variable lists


Syntax
---

```
syntax miModel [if] [in] [pw aw fw iw/], Ivar(varlist) Timevar(varname) /// 
							[COMMONcov(string asis) MODel(string asis)  ///
							CONDImputed(string asis) CONDComplete(string asis)  ///
							CATCutoff(integer 10)  MINCsize(integer 0) ///
							NAcode(integer -9999999) ///
							MIOptions(string asis) MERGOptions(string asis) ///
							SAVEmidata(string) PRINTmodel suspend debug] 
```
<br>

`pchained` takes the following arguments:

**Required**

| argument    | description            |
|-------------|------------------------|
| *miModel*   | models for scales or stand-alone variables to be imputed; see below for specific syntax |
| *Ivar*      | unique cluster/panel identifier (i.e. person, firm, country id) |
| *Timevar*   | time/wave identifier |

<br>

**Optional and conditionally required arguments:**

| argument       | description            |
|----------------|------------------------|
| *COMMONcov*    | covariates to be included in the scale item and stand-alone variable imputatation models, supports factor variable syntax and wild cards|
| *MODel*        | model and options to be passed on to `mi impute chained` for every imputed scale and stand-alone variable; this is a conditionally required argument; see below for details |
| *CONDImputed*  | conditional imputation; corresponds to `cond()` in `mi impute chained`; see below for syntax |
| *CONDComplete* | imputation conditional on the values of a exogenous and complete regressor (specified in `COMMONcov`); see below for syntax |
| *CATCutoff*    | maximum number of categories/levels to classify as categorical; if higher --> classified as continuous |
|                | default: `10` |
| *MINCsize*     | minimum cell size required for item to be included in analysis; if lower --> classified as rare |
|                | default: `0` |
| *NAcode*       | specifies the code used to label not applicable observations (applies to imputation subject to conditions) |
| *MIOptions*    | `mi impute chained` options to be passed on (`by()` is also allowed) |
| *MERGOptions*  | merge options to be passed on to `merge` upon merging the imputed data with the original data; imputed dataset is *master*, original dataset is *using* |
|                | default: `keep(match)` |
| *SAVEmidata*   | save the mi data; valid path and filename required |
| *PRINTmodel*   | print the imputation model |
| *suspend*      | suspend `pchained` immediately before imputation |

<br>

`fweight`, `aweight`, `pweight` and `iweight` are allowed. However, different `mi impute chained` models may 
impose restrictions. Please, see the help file of `mi impute chained` for further guidance.

<br>

**Syntax for `miModel`**

`pchained` could be used for imputing scale items and/or stand-alone dependent variables (sadv's). 
The `miModel` has the following general form:

`(dv [covariateList][, options])`

<br>

Multiple models could be specified using the following syntax:

`(dv1 [covariateList1][, options1])[(dv2 [covariateList2][, options2]) ...]`

<br>

The arguments that `miModel` takes are:

- `dv`: an item scale or a stand-alone dependent variable to be imputed
- `covariateList`: an optional list of covariates to be included in the imputation equation of `dv`. 
- `options`: could be any set of:
	- `scale`: has to be included if `dv` is a scale
	- `cont` or `continuous`: if `dv` is scale, this options allows the user to request items to be
	treated as continuous (another possibility to do the same thing is to specify an appropriate 
	estimation method in `MODel`)
    - `include([other_sadv][other_scales][mean(other_scales)][sum(other_scales)])`: allows 
	the user to specify other stand-alone variables, `other_sadv`, other scales, `other_scales` as well as 
	mean and sum scores of other scales being imputed as predictors in the 
	imputation model for `dv`; if `include` is specified, option `noimputed` is assumed.
	If `other_sadv` or `other_scales` are specified, all periods of the stand-alone variables in `other_sadv` 
	or the items of the scales in `other_scales` are used as predictors in the imputation equation.
	If a `mean` or `sum` score is requested, the score for the time period corresponding to 
	the time period of `dv` is included as a regressor.
	- `omit(varlist)`: allows the user to remove covariates listed in `COMMONcov` 
	from the imputation equation for `dv`
	- `noimputed`: instructs Stata to remove all other imputed variables used as 
	predictors in the imputation equation for `dv`, except other time periods of `dv` and
	variables specified in `include` and `COMMONcov`.
	- other options specific to the imputation model

Option `MODel` is required for all stand-alone dependent variables and is optional for scales. 
If the user provides an entry for a scale, their input overrides the internal method in `pchained`.

The internal method corresponds to the `dv` type (but it is **not** necessarily 
appropriate in every use case):

| `dv` type      | method    |
|----------------|-----------|
| continuous     | `regress` |
| binary         | `logit`   |
| multi-category | `ologit`  |


<br>

**Imputation subject to conditions**

`pchained` supports conditioning on complete and exogenous regressors and imputed variables (i.e. conditional imputation). 
Conditioning on scale scores, means and sums, is also supported.

To condition on complete regressors, the user needs to specify option `CONDComplete`. 
The option has the following syntax:

`condc(scale_stub or depvar = "if condition" [scale_stub or depvar = "if condition"]...)` 

where `condition` is either a standard Stata condition or a condition which involves `means` or `sums` of scales. 
Multiple conditions for multiple scales/stand-alone varibles can be specified. See section Examples for illutrations.

To request conditional imputation, the user has to specify option `CONDImputed`. The option has the same syntax as 
`CONDComplete` but the variables that are being conditioned on must be imputed variables.

An important requirement for imputation subject to conditions is to ensure "that missing 
values of all conditioning variables [are] nested within missing values 
of the conditional variable" (Stata Multiple-Imputation Reference Manual Release 15, p. 161).
This requirement, which we will refer to as "the nesting condition" may be unclear to novice 
users and therefore we have provided a use case in the Examples section. Also, the user has to 
tell Stata the value which is being used to label non-applicable observations 
(i.e. observations that fall outside of the condition); this is done using 
option `NAcode`.


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
***  One scale  ***

*** Categorical items, no covariates
simdata 200 3
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
		 (y2, noimputed include(y3 mean(s1_i)) omit(x* y*)) ///
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

// assign a large number that can be replaces with missing after imputation
replace y5 = -9999999 if y4 < 0  
replace y6 = -9999999 if x5 < 0 
 
*** >>>>>>>
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#d ;
pchained (s1_i, include(mean(s5_i s6_i) sum(s2_i)) scale omit(x*)) 
		 (s2_i, include(mean(s1_i) sum(s5_i s6_i)) scale omit(x5_base))
		 (s5_i, include(mean(s1_i s2_i s6_i)) scale)
		 (s6_i, include(s1_i mean(s2_i s5_i)) scale omit(x5_base))
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
***  Imputing on complete scales (Plumpton bypass) ***

***		
simdata 500 3
pchained (s1_i, noimputed include(s3_i) scale) ///
		 (s3_i, noimputed include(s1_i) scale), ///
		 i(id) t(time) ///
		 common(x1 i.x2 x3 y1) ///
		 mio(add(1) chaindots rseed(123456) dryrun)
		 
```

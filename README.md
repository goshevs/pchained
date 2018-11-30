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
qui do "https://raw.githubusercontent.com/goshevs/pchained-github/devel-ef/pchained.ado"
```


Syntax
---

```
pchained scale_stubs [sadv_models] [if] [in] [weight], Ivar(varlist) Timevar(varname)
					   [CONTinous(namelist) SCOREtype(string asis) ///
						SCALECOVars(varlist fv) ADDSADepvars(varlist) /// 
						MIOptions(string asis) CATCutoff(integer 10) ///
						MINCsize(integer 0) MERGOptions(string asis) ///
						MODel(string asis) SAVEmidata(string)]
```
<br>

`pchained` takes the following arguments:

**Required**

| argument       | description            |
|----------------|------------------------|
| *scale_stubs*  | unique stub names of the scale(s) to be imputed (takes multiple scales) |
| *Ivar*         | unique cluster/panel identifier (i.e. person, firm, country id) |
| *Timevar*      | time/wave identifier |

<br>

**Optional and conditionally required arguments:**

| argument       | description            |
|----------------|------------------------|
| *sadv_models*  | models for stand-alone variables to be imputed; see below for specific syntax|
| *CONTinous*    | stub names of scales whose items should be treated as continuous |
| *SCOREtype*    | mean score or sum score |
|                | default: `mean`
| *SCALECOVars*  | list of covariates to be included in the scale item imputatation models, supports factor variable syntax  |
| *ADDSADepvars* | list of stand-alone variables to be included in the scale item imputation equations; all periods of these variables are used in the imputation equation for an item |
| *MIOptions*    | `mi impute chained` options to be passed on (`by()` is also allowed) |
| *CATCutoff*    | maximum number of categories/levels to classify as categorical; if higher --> classified as continuous |
|                | default: `10` |
| *MINCsize*     | minimum cell size required for item to be included in analysis; if lower --> classified as rare |
|                | default: `0` |
| *MERGOptions*  | merge options to be passed on to `merge` upon merging the imputed data with the original data; imputed dataset is *master*, original dataset is *using* |
|                | default: `keep(match)` |
| *MODel*        | user can pass a model and options to `mi impute chained` for each imputed scale and stand-alone variable; this is a conditionally required argument; see below for details |
| *SAVEmidata*   | save the mi data; valid path and filename required |

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

Working with sensitive data?
---

If you are working with sensitive data, please ensure that you point Stata to a secure
directory that it can use as a temporary directory. Please, see [this](https://www.stata.com/support/faqs/data-management/statatmp-environment-variable/) reference for 
instructions on how to do this.

<br>

Examples
---

```
*******************
***  One scale  ***

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


*******************
*** Two scales  ***

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


********************
*** Three scales ***

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


********************
***   By group   ***

simdata 1000 3
pchained s1_i s4_i, i(id) t(time) scalecov(x1 i.x2 x3 y1) score(sum) mio(add(1) chaindots by(group) rseed(123456))


*************************
***  Sampling Weight  ***

simdata 1000 3
pchained s1_i s4_i [pw=weight], i(id) t(time) scalecov(x1 i.x2 x3 y1) score(sum) mio(add(1) chaindots rseed(123456))


****************************************************************
***  Imputing non-scale variables together with scale items  ***

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


```

* Plumpton with -mi impute chained-
* Author: Simo Goshev
* Date: 7/10/2018
* Version: 0.1
*
*
*



********************************************************************************
*** Data
********************************************************************************

capture program drop simdata
program define simdata

	clear
	set more off
	set seed 123456

	********************************************************************************
	*** Generate dummy data
	********************************************************************************

	set obs 200
	gen id = _n
	expand 3
	bys id: gen time = _n

	bys id: gen x1 = round(5 + 5 * runiform()) if _n == 1
	bys id: replace x1 = x1[_n -1] + 1 if _n > 1

	bys id: gen ih = rnormal() if _n ==1
	bys id: replace ih = ih[1] if _n > 1

	** Time-invariant categorical var
	bys id: gen x2 = round(1 + 4*runiform() + ih) if _n ==1
	bys id: replace x2 = cond(x2[1] < 0, 0, x2[1])
	bys id: replace x2 = x2[1] if _n > 1

	** Time-variant categorical var
	bys id: gen x3 = round(1 + 3*runiform() + ih) if _n ==1
	bys id: replace x3 = cond(x3[1] < 0, 0, x3[1])
	bys id: replace x3 = cond(runiform() > 0.3, x3[_n -1] + 1, x3[_n-1]) if _n > 1 


	bys id: gen x4 = round(1 + 3*runiform() + ih) if _n ==1
	bys id: replace x4 = cond(x4[1] < 0, 0, x4[1])
	bys id: replace x4 = cond(runiform() > 0.3, x4[_n -1] + 1, x4[_n-1]) if _n > 1 



	** Scale 1:
	forval i = 1/3 {
		bys id: gen s1_i`i' = floor(2 * runiform() + ih) 
		replace s1_i`i' = 0 if s1_i`i' < 0
	}

	** Scale 2:
	forval i = 1/2 {
		bys id: gen s2_i`i' = floor(7 * runiform() + ih) 
		replace s2_i`i' = 0 if s2_i`i' < 0
	}


	** Scale 3:
	forval i = 1/4 {
		bys id: gen s3_i`i' = floor(2 * runiform() + ih) 
		replace s3_i`i' = 0 if s3_i`i' < 0
	}


	gen y = 0.5 + 1.5 * x1 - 0.5 * x2 + 1.5 * x3 + ih + rnormal()

	drop ih

	sum


	********************************************************************************
	*** Introduce missingness
	********************************************************************************

	foreach var of varlist s* {
		replace `var' = . if runiform() <0.1
	}

	misstable sum s*

end
***






********************************************************************************
*** Program
********************************************************************************

*** We start with data in long format
capture program drop mywrapper
program define mywrapper, eclass

	syntax namelist [if], Panelvar(varlist) Timevar(varlist) /// 
						 [MType(string) SCOREtype(string) COVars(varlist fv) ///
						  MIOptions(string) SAVEmidata(string)]
	
	qui {
		*** namelist = unique stub names of the scale(s) to be imputed (takes multiple scales)
		*** panelvar = cluster identifier (person, firm, country)
		*** timevar  = time identifier
		*** covars  = list of covariates
		*** scoretype = mean or sum
		*** mioptions = mi impute chained options to be passed on
		*** mtype = type of imputation model (not yet done by var!)
		*** savemidata = save the mi data if desired
		
		
		**** Specification of default values
		*** Default scoretype to mean
		if "`scoretype'" == "" local scoretype "mean"
		*** Default mioptions
		if "`mioptions'" == "" local mioptions "add(5)"
		*** Default model type
		if "`mtype'" == "" local mtype "logit" // "mlogit", "auto"
	
		
		tempfile originaldata
		save "`originaldata'", replace
			
		* preserve
		
		*** Collect all items of all scales
		local i = 1
		local allitemsrs ""  // collection of all items (renamed for reshape)
		foreach scale of local namelist {
			capture unab myitems_`i': `scale'*
			*** check whether elements of namelist are variables
			if _rc != 0 {
				di in r "Stub `scale' is not associated with a scale"
				exit 111
			}
			*** rename variables in dataset and in the locals to facilitate reshape
			foreach item of local myitems_`i' {
				local allitemsrs "`allitemsrs' `item'_`timevar'" // can possibly break if too many items
				ren `item' `item'_`timevar'
			}
			local ++i
		}
			
		*** Prepare covariates for reshape (remove any fvvar indicators)
		if "`covars'" ~= "" {
			***  TODO: Better syntax here would be remove everything between a space and a period,
			*** including the period!!!
			local covarsrs "`=subinstr("`covars'", "i.","",.)'"
		}
		
		*** Collect the level of timevar
		levelsof `timevar', local(timelevs)
		
		*** Keep only variables of interest	
		keep `allitemsrs' `covarsrs' `panelvar' `timevar'
		
		*** Reshape to wide
		
		noi di _n in y "Reshaping to wide..."
		reshape wide `allitemsrs' `covarsrs', i(`panelvar') j(`timevar')
		* order _all, alpha  // useful for debugging
		
		*** Build syntax for mi impute chained ***
		local mymisyntax ""
		foreach el of local namelist {  // loop over scales
			unab myscale: `el'*
			
			*** create the expressions for --include-- from remaining scales 
			local remaining = trim(subinstr("`namelist'", "`el'","", .))
			local include_items ""
			
			if "`remaining'" ~= "" {
				*** Compute aggregates by the levels of timevar
				foreach remscale of local remaining {
					unab myitems: `remscale'*
					foreach tlev of local timelevs {
						local taggregs ""
						foreach item of local myitems {	
							if regexm("`item'", "^`remscale'[a-z0-9]*_`timevar'`tlev'$") {
								local taggregs "`taggregs' `=regexs(0)'"
							}
						}
						* noi di "`taggregs'"	
						*** This is where we write out the functions
						local mysum "(`=subinstr("`=trim("`taggregs'")'", " ", "+", .)')"
						if "`scoretype'" == "sum" {
							local include_items "`include_items' (`mysum')"
						}	
						else if "`scoretype'" == "mean" {
							local nitems: word count `taggregs'	
							local include_items "`include_items' (`mysum'/`nitems')"
						}
						else {
							di in r "`scoretype' is not allowed as a score type"
							exit 198
						}
					}
				}
				local include_items "`include_items'"
				
				
			}
			* noi di "`include_items'"
			
			
			*** write out the imputation models
			foreach depvar of local myscale {
				local rhs_imputed = trim(subinstr("`myscale'", "`depvar'", "", .))
				
				*** Include imputed variables in parenthesis
				local rhs_imputed_pr ""
				foreach rhs of local rhs_imputed {
					local rhs_imputed_pr "`rhs_imputed_pr' (`rhs')"
				}
				**** TODO: Here can test the var and adjust the mtype respectively (binary vs categorical)!!!
				local mymodel "`mymodel' (`mtype', augment noimputed include(`include_items' `rhs_imputed_pr')) `depvar' " //can break of too many items*time-periods
			}
		}
		
		*** If covariates are (not) present
		if "`covars'" ~= "" {
					
			*** Build list of covariates in wide format
			foreach cov of local covars {
				fvunab mycov: `cov'*
				local covars_wide "`covars_wide' `mycov'"
			}
		
			*** write out the exogenous vars and mi options
			local model_endpart "= `covars_wide', `mioptions'"
		}
		else {
			local model_endpart ", `mioptions'"
		}
		
		*** Write out the complete model
		local model_full "`mymodel' `model_endpart'"
		* di "`model_full'"  // useful for debigging
			
		*** mi set the data
		mi set flong
		foreach scale of local namelist {
			mi register imputed `scale'*
		}
			
		*** mi impute chained
		noi di _n in y "Performing multiple imputation..."
		noi mi impute chained `model_full'
		
		*** reshape to long
		mi reshape long `allitemsrs' `covarsrs', i(`panelvar') j(`timevar')
		
		*** rename vars to original names
		foreach var of varlist `allitemsrs' {
			ren `var' `=subinstr("`var'", "_`timevar'","",.)'
		}
		
		*** Save the data
		if "`savemidata'" ~= "" {
			noi di _n in y "Saving mi dataset..."
			save "`savemidata'", replace
		}
		
		* restore
		
		*** Merge the midata into the original dataset
		*mi set flong
		noi di _n in y "Merging imputed dataset with original dataset..."
		noi mi merge m:1 `panelvar' `timevar' using "`originaldata'", keep(match)
		*mi merge 1:m `panelvar' `timevar' using "`savemidata'", keep(match)
		mi update
		
		noi di _n in y "All done!"
	}
end
****


** Short syntax 1
** mywrapper s1_i s2_i, panelvar(id) timevar(time)
** Short syntax 2
* mywrapper s1_i s2_i, p(id) t(time) cov(x1 i.x2 x3 y) score("mean") mio("add(1)")
** Full syntax
** mywrapper s1_i s2_i, panelvar(id) timevar(time) covars(x1 i.x2 x3 y) scoretype("mean") mioptions("add(1)")


** Examples
simdata
mywrapper s1_i, p(id) t(time) cov(x1 i.x2 x3 y) score("sum") mio("add(1)")
simdata
mywrapper s1_i s2_i, p(id) t(time) cov(x1 i.x2 x3 y) score("sum") mio("add(1)")
simdata
mywrapper s1_i s2_i s3_i, p(id) t(time) cov(x1 i.x2 x3 y) score("sum") mio("add(1)")


*mi xeq: egen s1_sum = rowtotal(s1*)
*mi xeq: egen s1_mean = rowmean(s1*)



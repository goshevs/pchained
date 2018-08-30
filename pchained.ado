* Plumpton with -mi impute chained-
* Authors: Simo Goshev, Zitong Liu
* Date: 8/27/2018
* Version: 0.22
*
*
*

*
********************************************************************************
*** Program
********************************************************************************

*** We start with data in long format
capture program drop pchained
program define pchained, eclass

	syntax namelist [if], Panelvar(varlist) Timevar(varname) /// 
						 [MType(string) SCOREtype(string) COVars(varlist fv) ///
						  MIOptions(string) SAVEmidata(string)]
	** TODO: add parsing for by() in mi impute chained mioptions (basically to keep the vars)

	*** Warn user they need moremata
	no di in gr "Warning: this program requires package moremata."

	qui {
		*** namelist   = unique stub names of the scale(s) to be imputed (takes multiple scales)
		*** panelvar   = cluster identifier (person, firm, country)
		*** timevar    = time identifier
		*** covars     = list of covariates
		*** scoretype  = mean or sum
		*** mioptions  = mi impute chained options to be passed on
		*** mtype      = type of imputation model (?TODO?: not yet done by var!)
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
																 // ?TODO: transfer over to Mata?
				ren `item' `item'_`timevar'
			}
			local ++i
		}
			
		*** Prepare covariates for reshape
		if "`covars'" ~= "" {

			*** Remove factor syntax from variables
			local covarsrs ""
			foreach covar of local covars {
				local covarrs "`=regexr("`covar'", "^[a-zA-z0-9]+\.", "")'"
				local covarsrs `covarsrs' `covarrs'			
			}
			
			*** Separate time invariant from time variant covariates
			local cov_invar ""
			local cov_var ""
			foreach covar of local covarsrs {
				tempvar mytest
				sort `panelvar' `timevar'
				bys `panelvar': egen `mytest' = mean(`covar')
				capture assert `mytest' == `covar'
				if _rc ~= 0 {
					local cov_var "`cov_var' `covar'"
				}
				else {
					local cov_invar "`cov_invar' `covar'"
				}
			}	
		}
		
		*** Report covariates
		noi di _n in y "********************************************************"
		noi di in y "Covariates: "
		noi di in y "    Time-invariant: `cov_invar'"
		noi di in y "    Time-variant  : `cov_var'"
		noi di "********************************************************"		
		
		*** Collect the level of timevar
		levelsof `timevar', local(timelevs)
		
		*** Keep only variables of interest	
		keep `allitemsrs' `covarsrs' `panelvar' `timevar'
		
		*** Reshape to wide
		noi di _n in y "Reshaping to wide..."
		reshape wide `allitemsrs' `cov_var', i(`panelvar') j(`timevar')
		* order _all, alpha  // useful for debugging
		
		
		
		*** Check item type as well as constant items and rare categories
		foreach el of local namelist {  // loop over scales
		
			local bin  "" // binary items
			local cat  "" // multiple category items
			local cont "" // continuous items
			
			local finalScale ""   // admitted items
			local constant ""        // constant items
			local rare ""            // items with rare categories 
			
			*** Collect all items of the scale 
			unab myscale: `el'*
			
			foreach item of local myscale {  //iterate over items of scales
				levelsof `item', local(levs) // PROBLEMATIC; NEED TO CHANGE
				if (`:word count `levs'' == 1)  {
					local constant "`constant' `item'" 
				}
				else {
					tab `item', matcell(freqs)   // PROBLEMATIC; NEED TO CHANGE
					if (`r(r)' == 2 ) { // binary
						local bin "`bin' `item'"
					}
					if (`r(r)' < 10 ) {   // item is categorical; HARD CODED NEED TO CHANGE
						local cat "`cat' `item'"
						mata: st_numscalar("pCats", colsum(mm_cond(st_matrix("freqs") :< 0, 1,0)))  // 10 obs per cat; HARD CODED NEED TO CHANGE
						
						if (pCats > 0) {
							local rare "`rare' `item'"
						}
						else {
							local finalScale "`finalScale' `item'"
						}
					}
					else {
						local cont "`cont' `item'"
						local finalScale "`finalScale' `item'" //accumulate admissible items
					}
				}
			} // end loop over items
			
			
			* noi di "`finalScale'"
			
			*** Report results by scale
			noi di _n "********************************************************" _n ///
			"Summary of pre-imputation checks for scale `el'*" _n  ///
			"Constant items: `constant'" _n ///
			"Binary items: `bin'" _n ///
			"Multiple category items: `cat'" _n ///
			"Continuous items: `cont'" _n ///
			"Excluded items: " _n ///
			"      Constant items: `constant'" _n ///
			"      Categorical items with < 0 obs in a category: `rare'"
						
			noi di in y _n "Filtered scale: `finalScale'"
			noi di "********************************************************" _n
				
			
			*** create the expressions for --include-- from remaining scales 
			local remaining = trim(subinstr("`namelist'", "`el'","", .))
			* noi di "`remaining'"
			
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
				
			} // end of remaing

			* no di "`finalScale'"
			* no di "`include_items'"
			
			*** write out the imputation models
			foreach depvar of local finalScale {
				local rhs_imputed = trim(subinstr("`finalScale'", "`depvar'", "", .))
				
				*** Include imputed variables in parenthesis
				local rhs_imputed_pr ""
				foreach rhs of local rhs_imputed {
					local rhs_imputed_pr "`rhs_imputed_pr' (`rhs')"
				}
				**** TODO: Here can test the var and adjust the mtype respectively (binary vs categorical vs continuous)!!!
				
				**** Note by Zitong: 
				**** Give mtype 2 choice, manually overriding, and automatic way. 
				**** continuous variable: regress (never try pmm, the STATA default realization of pmm always leads mistake). No augment
				**** binary: logit. Use augment option
				**** Ordered categorical variable: ologit, use augment option
				**** No ordered categorical variable: mlogit, use augment option
				
				local mymodel "`mymodel' (`mtype', noimputed augment include(`include_items' `rhs_imputed_pr')) `depvar' " //can break of too many items*time-periods
			}
		}   // end of quietly
	} // end of loop over scales
	
	*** If covariates are (not) present
	if "`covars'" ~= "" {

		*** Build list of covariates in wide format
		foreach cov of local covars {
			fvunab mycov: `cov'*
			local covars_wide "`covars_wide' `mycov'"
		}
		* noi di "`covars_wide'"
		
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
	mi reshape long `allitemsrs' `cov_var', i(`panelvar') j(`timevar')
	
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
	
	noi di _n in y "Imputation finished."
	
end

* Plumpton with -mi impute chained-
* Author: Simo Goshev
* Date: 7/12/2018
* Version: 0.1
*
*
*

*** TODO: add parsing for by() in mi impute chained mioptions (basically keep the vars)


********************************************************************************
*** Program
********************************************************************************

*** We start with data in long format
capture program drop pchained
program define pchained, eclass

	syntax namelist [if], Panelvar(varlist) Timevar(varname) /// 
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
			
			*** Separate time invariant from time variant covariates
			local cov_invar ""
			local cov_var ""
			foreach covar of local covarsrs {
				tempvar mytest
				bys `panelvar' `timevar': egen `mytest' = mean(`covar')
				capture assert `mytest' == `covar'
				if _rc ~= 0 {
					local cov_var "`cov_var' `covar'"
				}
				else {
					local cov_invar "`cov_invar' `covar'"
				}
			}	
		}
		
		*** Collect the level of timevar
		levelsof `timevar', local(timelevs)
		
		*** Keep only variables of interest	
		keep `allitemsrs' `covarsrs' `panelvar' `timevar'
		
		
		*** Reshape to wide
		noi di _n in y "Reshaping to wide..."
		reshape wide `allitemsrs' `cov_var', i(`panelvar') j(`timevar')
		* order _all, alpha  // useful for debugging
		
		*** Build syntax for mi impute chained ***
		local mymisyntax ""
		foreach el of local namelist {  // loop over scales
			unab myscale: `el'*
			
			********************************************************************
			*** Item checks
			
			*** Check for items that do not vary and exclude them
			*** Check for items that have rare events
			local mynewscale ""
			local constant ""
			local rare ""
			noi di _n "Pre-imputation check of variables..."
			foreach item of local myscale {
				sum `item'
				if (`r(min)' ~= `r(max)') & (`r(mean)' > 0.3 & `r(mean)' < 0.7) {
					local mynewscale "`mynewscale' `item'"
				}
				else if (`r(min)' == `r(max)') {
					local constant "`constant', `item'"
					noi di "Warning: `item' excluded because it does not vary"
				}
				else {
					local rare "`rare', `item'"
					noi di "Warning: `item' excluded because of rare " ///
					"0's (`=round((1-`r(mean)')*`=_N')'/`=_N') or " ///
					"1's (`=round(`r(mean)'*`=_N')'/`=_N')"
					
				}
				
			}

			********************************************************************
				
			*** Replace myscale with mynewscale
			local myscale "`mynewscale'"
			
			* noi di _n "`myscale'"
			
			/*
			foreach var of local myscale {
				noi tab `var'
			}
			*/
			
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
				local mymodel "`mymodel' (`mtype', noimputed augment include(`include_items' `rhs_imputed_pr')) `depvar' " //can break of too many items*time-periods
			}
		}
		
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
		
		noi di _n in y "All done!"
	}
end

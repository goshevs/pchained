* Plumpton with -mi impute chained-
* Authors: Simo Goshev, Zitong Liu
* Date: 8/6/2018
* Version: 0.21
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
						  MIOptions(string) SAVEmidata(string) TABle VALCheck]
	** TODO: add parsing for by() in mi impute chained mioptions (basically to keep the vars)

	*** Check if required packages are installed
	cap which distinct
	if _rc {
		no error "This program requires package distinct. Please, install before proceeding."
		
	}
				  
	qui {
		*** namelist   = unique stub names of the scale(s) to be imputed (takes multiple scales)
		*** panelvar   = cluster identifier (person, firm, country)
		*** timevar    = time identifier
		*** covars     = list of covariates
		*** scoretype  = mean or sum
		*** mioptions  = mi impute chained options to be passed on
		*** mtype      = type of imputation model (?TODO?: not yet done by var!)
		*** savemidata = save the mi data if desired
		*** table      = print table of imputation variables
		*** valcheck   = conduct a 0.3 < mean <0.7 test (for binary vars)
		
		
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
			
		*** Prepare covariates for reshape (remove any fvvar indicators)
		if "`covars'" ~= "" {
			***  TODO: Better syntax here would be remove everything between a space and a period. 
			**** BY ZITONG: DONE. Though iteration seems awkward, there is no way in STATA that can avoid iteration. 
			**** Assumption: What is before the dot is subtring that consists only characters and numbers.
			**** Note Now allows both cases:
			**** 1. Multiple ***.**** type of covars. 
			**** 2. Start with ***.**** type of covars. 
			**** TODO: cannot work if we have i.x1 and d.x1, you will get 2 x1 later. (think of put the earlier part to the end. )
			
			*** OLD SYNTAX
			** local covarsrs "`=subinstr("`covars'", "i.","",.)'"
			
			*** NEW SYNTAX
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
		
		noi di "Invariant: `cov_invar'"
		noi di "Variant: `cov_var'"
					
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
			
			if "`valcheck'" ~= "" {
				noi di _n "Pre-imputation check of variables..."
			}
			else {
				noi di _n "Pre-imputation check for variables that do not vary..."
			}
			
			******** ZITONG's Note: 
			**** About binary variables, categorical variables and continuous variables. 
			**** We need more clear definition to define them: We need to know how the scales are constructed. 
			**** Here are some reasons:
			**** 1. Binary variable: Simo's rareness test assumes that binary variable has only 0 and 1. So there is no case
			**** as in other datasets, in which variable "sex", a binary variable, has two values "1" and "3". 
			**** 2. What is a categorical variable? What's the tolarance for "categories"? Smaller than 30, or something? 
						
			**** Temporary Definition of BINARY VARIABLE: 
			**** Any variable with only 0 and 1 is binary. If a variable have 2 values but not exactly 0 and 1, it will also
			**** be defined as a categorical variable. 
			
			**** Temoporary Definition of CONTINUOUS VARIABLE:
			**** Distinct levels are more than half of the total nonmissing observations. 
			
			**** Temoporary Definition of CATEGORICAL VARIABLE: 
			**** Not defined as binary or continuous. Not constant. 
			
			**** ADDED PART: display the type of each scale variable. 
			**** QUESTION: only to scale? not covar? ( RIght now I assume only for scale) 
			
			**** Literally this part should put earlier but now let's put it here to be clearer. 
			**** Maybe also should define a manual input as valcheck. Put ont TODO list. 
			
			local biscale "" // Binary variable names
			local catscale "" // Catogorical variable names
			local contscale "" // Continuous variable names
						
			foreach item of local myscale {
				levelsof `item' 
				if (r(levels) == "0 1")  {
					local biscale "`biscale' `item'"
					if "`valcheck'" == "" {
						local mynewscale "`mynewscale' `item'"
					}
					else {
						summarize `item'
						if (`r(mean)' > 0.3 & `r(mean)' < 0.7) {
							local mynewscale "`mynewscale' `item'"
						}
						else {
							local rare "`rare', `item'"
							noi di "Warning: `item' excluded because of rare " ///
							"0's (`=round((1-`r(mean)')*`r(N)')'/`r(N)') or " ///
							"1's (`=round(`r(mean)'*`r(N)')'/`r(N)')"
							}		
					}					
				}
					
				else {
					distinct `item'
					scalar distvals = r(ndistinct)
					if  ( distvals == 1) {
						local constant "`constant', `item'"
						noi di "Warning: `item' excluded because it does not vary"
					}	// Constant
					else { // Decide continuous or not. 
						count if !missing(`item')
						scalar totnonmis = r(N)
						scalar ratio = distvals/totnonmis
						if (ratio > 0.5) { // Continous Variable
							local mynewscale "`mynewscale' `item'"
							local contscale "`contscale' `item'"
							noi di "`item' is a continous variable. Pass the pre-imputation check. "
						}
						else { // In the end, Categorical variables. 
						**** Don't have time to complete this, I put it into my TODO list. 
						
							local catscale "`catscale' `item'"
							local mynewscale "`mynewscale' `item'"
							**** Use levelsof and count for different values. 
							**** Usually One scale has same options for different subscales
							**** Different scales have different options. 
							**** Maybe I want to report a table here. 
							
						}
					}
				}
			}
			
			**** Report a summary of pre-imputation check results: 
			noi di "Pre-imputation check is done! Here is a summary:  " _newline  ///
				"Constant Scales: `constant'" _newline ///
				"Binary Scales: `biscale'" _newline ///
				"Categorical Scales: `catscale'" _newline ///
				"Continuous Scales: `contscale'" _newline ///
				"Excluded by pre-imputation check: `constant' `rare'" _newline ///
				"Pass the pre-imputation check: `mynewscale'"
					
					
			/*
			foreach item of local myscale {
				sum `item'
				if (`r(min)' ~= `r(max)') {  // & (`r(mean)' > 0.3 & `r(mean)' < 0.7) { // this is only for binary vars! TODO: subroutine for others
					if "`valcheck'" == "" {
						local mynewscale "`mynewscale' `item'"
					}
					else {
						if (`r(mean)' > 0.3 & `r(mean)' < 0.7) {
							local mynewscale "`mynewscale' `item'"
						}
						else {
							local rare "`rare', `item'"
							noi di "Warning: `item' excluded because of rare " ///
							"0's (`=round((1-`r(mean)')*`r(N)')'/`r(N)') or " ///
							"1's (`=round(`r(mean)'*`r(N)')'/`r(N)')"
						}
					}
				}
				else {
					local constant "`constant', `item'"
					noi di "Warning: `item' excluded because it does not vary"
					
				}
				*/
				
/*					
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
*/
	
			}

			********************************************************************
				
			*** Replace myscale with mynewscale
			local myscale "`mynewscale'"
			
			* noi di _n "`myscale'"
			
			if "`table'" ~= "" {
				foreach var of local myscale {
					noi tab `var'
				}
			}
			
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
				**** TODO: Here can test the var and adjust the mtype respectively (binary vs categorical vs continuous)!!!
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

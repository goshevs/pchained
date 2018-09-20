* Plumpton with -mi impute chained-
* Authors: Simo Goshev, Zitong Liu
* Version: 0.25
*
*
*
*
*
*
*** SYNTAX ***
*** namelist   = unique stub names of the scale(s) to be imputed (takes multiple scales)
*** Panelvar   = cluster identifier (i.e. person, firm, country id)
*** Timevar    = time/wave identifier
*** CONTinous  = stub names of scales whose items should be treated as continuous
*** SCOREtype  = mean score (default) or sum score
*** COVars     = list of covariates, supports factor variable syntax 
*** MIOptions  = mi impute chained options to be passed on (by() is also allowed)
*** SAVEmidata = save the mi data; need to valid provide path and filename
*** CATCutoff  = max number of categories/levels to classify as categorical; if fails --> classified as continuous
*** MINCsize   = minium cell size required for item to be included in analysis; if fails --> classified as rare
		


********************************************************************************
*** Program
********************************************************************************

*** We start with data in long form
capture program drop pchained
program define pchained, eclass

	syntax namelist [if], Panelvar(varlist) Timevar(varname) /// 
						 [CONTinous(namelist) SCOREtype(string) ///
						  COVars(varlist fv) MIOptions(string) ///
						  SAVEmidata(string) CATCutoff(integer 10) ///
						  MINCsize(integer 0)]

						  
	*** Warn user they need moremata
	no di in gr "Warning: this program requires package moremata."

	qui {

		**** Specification of default values
		*** Default scoretype to mean
		if "`scoretype'" == "" local scoretype "mean"

		*** Default mioptions
		if "`mioptions'" == "" {
			local mioptions "add(5)"
		} // if not empty, check for by and retrieve the by varname
		else if regexm("`mioptions'", "by\([a-zA-Z0-9]+\)") {
			local myby "`=regexs(0)'"
			gettoken left gr: myby, parse("(")
			gettoken left gr: gr, parse("(")
			gettoken byGroup right: gr, parse(")")
		}
		
		tempfile originaldata
		save "`originaldata'", replace
			
		* preserve
				
		
		*** Collect all items of all scales for reshape
		local allitemsrs ""  // collection of all items (renamed for reshape)
		foreach scale of local namelist {
			capture unab myitems: `scale'*
			*** check whether elements of namelist are variables
			if _rc != 0 {
				di in r "Stub `scale' is not associated with a scale"
				exit 111
			}
			*** rename variables in dataset and in the locals to facilitate reshape
			foreach item of local myitems {
				local allitemsrs "`allitemsrs' `item'_`timevar'" // may break if too many items
				ren `item' `item'_`timevar'
			} // End loop over myitems_i
						
			**** Feed in variable type information
			local isCont: list scale in continous
			if `isCont' == 1 { 
				local userDefCont "`userDefCont' `scale'"
			} 
		}
		* noi di "`userDefCont'"
		
		*** Prepare covariates for reshape
		if "`covars'" ~= "" {

			*** Extract covariate names from covariate list (which may be fvvarlist)
			fvrevar `covars', list
			local covarsrs "`r(varlist)'"
			* noi di "`covarsrs'"
		
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
		
			*** Report covariates
			noi di _n in y "********************************************************"
			noi di in y "Covariates: "
			noi di in y "    Time-invariant: `cov_invar'"
			noi di in y "    Time-variant  : `cov_var'"
			noi di "********************************************************"		
		}
		else {
			noi di _n in y "********************************************************"
			noi di in y "No covariates included in the imputation model "
			noi di "********************************************************"		
		}
		
		*** Collect the level of timevar
		levelsof `timevar', local(timelevs)
		
		*** Keep only variables of interest	
		keep `allitemsrs' `covarsrs' `panelvar' `timevar' `byGroup'
		
		*** Reshape to wide
		noi di _n in y "Reshaping to wide..."
		reshape wide `allitemsrs' `cov_var', i(`panelvar') j(`timevar')
		* order _all, alpha  // useful for debugging
		
		** We are to impute on WIDE variables. 
		
		*** Specify temp names for scalars and matrices
		tempname vals freqs pCats nCats
		
		foreach el of local namelist {  // loop over stubs
	
			*** Check item type as well as constant items and rare categories
		
			local bin  "" // binary items
			local cat  "" // multiple category items
			local cont "" // continuous items
			
			local finalScale ""   // admitted items
			local constant ""        // constant items
			local rare ""            // items with rare categories 
			local cuscont ""		// Items designated as continous by user

			*** Collect all items of the scale
			unab myscale: `el'*

			*** User assignment to continuous
			local userOverride: list el in userDefCont // is scale in user-defined?
			* noi di "Override: `userOverride'"
			if (`userOverride' == 1) {
				foreach item of local myscale {  //iterate over items of user-defined
					capture tab `item', matrow(`vals')
					if (_rc == 0) {  // if does not break
						mata: st_numscalar("`nCats'", rows(st_matrix("`vals'"))) // number of categories
						if (`nCats' == 1)  {
							local constant "`constant' `item'" 
						}
						else { 
							local cuscont "`cuscont' `item'"  // add to customized continous vars
							local finalScale "`finalScale' `item'"
						}
					}
					else if (_rc = 134) {
						local cuscont "`cuscont' `item'"  // add to customized continous vars
						local finalScale "`finalScale' `item'"
					}
					else {
						di in r "Cannot classify `item'"
						exit 1000
					}	
				}
			}
			else {			
				*** Automatic assignment to all various types
				foreach item of local myscale {  //iterate over items of scales
					capture tab `item', matrow(`vals') matcell(`freqs')
					if (_rc == 0) {  // if does not break tab
						mata: st_numscalar("`nCats'", rows(st_matrix("`vals'"))) // number of categories
						if (`nCats'  < `catcutoff') {  // item is categorical
							if (`nCats'  == 1)  {
								local constant "`constant' `item'" 
							}
							else {  // more than 1 categories
								mata: st_numscalar("`pCats'", colsum(mm_cond(st_matrix("`freqs'") :< `mincsize', 1,0))) // min # of obs per cat 
								**** IMPORTANT Zitong's Note. This might be problematic because 
								****   there may exists some "rare categories" while we have enough points for other categories. 
								if (`pCats' > 0) {
									local rare "`rare' `item'"
								}
								else {   // if not rare
									if (`nCats' == 2) { // Binary
										local bin "`bin' `item'"
									}
									else {  // Multi-category
										local cat "`cat' `item'"
									}
									local finalScale "`finalScale' `item'"
								}
							}
						} // end of if  
						else { // item is continuous
							local cont "`cont' `item'"
							local finalScale "`finalScale' `item'" // Continous vars pass directly
						}
					} //end of _rc == 0
					else if (_rc == 134)  { // item is continuous
						local cont "`cont' `item'"
						local finalScale "`finalScale' `item'" // Continous vars pass directly
					}
					else {
						di in r "Cannot classify `item'"
						exit 1000
					}
				} // end loop over items
			} // and of else in userOverride
			
			* noi di "`finalScale'"
			
			*** Report results by scale
			noi di _n "********************************************************" _n ///
			"Summary of pre-imputation checks for scale `el'*" _n  ///
			"Constant items: `constant'" _n ///
			"Binary items: `bin'" _n ///
			"Multiple category items: `cat'" _n ///
			"Continuous items: " _n ///
			"      Auto detected: `cont'" _n ///
			"      User defined : `cuscont'" _n ///
			"Excluded items: " _n ///
			"      Constant items: `constant'" _n ///
			"      Categorical items with < `mincsize' obs in a category: `rare'"
						
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

				**** Note by Zitong: 
				**** Give mtype 2 choice, manually overriding, and automatic way. 
				**** continuous variable: regress (never try pmm, the Stata default realization of pmm always leads mistake). No augment
				**** binary: logit. Use augment option
				**** Ordered categorical variable: ologit, use augment option
				
				if `: list depvar in bin' {
					local mymodel "`mymodel' (logit, noimputed augment include(`include_items' `rhs_imputed_pr')) `depvar' "
				}
				else if `: list depvar in cat' {
					local mymodel "`mymodel' (ologit, noimputed augment include(`include_items' `rhs_imputed_pr')) `depvar' "
				}
				else {
					local mymodel "`mymodel' (reg, noimputed include(`include_items' `rhs_imputed_pr')) `depvar' "				
				}
			} // end of foreach
		}   // end of loop over scales

	
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
	} // end of quietly
	
end

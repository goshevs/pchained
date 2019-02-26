* Plumpton with -mi impute chained-
* Authors: Simo Goshev, Zitong Liu
* Version: 1.0
*
*
*
*
*
*

*** SYNTAX ***
*** anything     = unique stub names of the scale(s) to be imputed (takes multiple scales)
*** Ivar         = cluster identifier (i.e. person, firm, country id)
*** Timevar      = time/wave identifier
*** CONTinous    = stub names of scales whose items should be treated as continuous
*** SCOREtype    = mean score (default) or sum score
*** COMCOVars    = list of covariates used for scale and stand-alone variable imputation, supports factor variable syntax 
*R** ADDSADepvars = list of stand-alone varaibles to be imputed together with the scale items
*** MIOptions    = mi impute chained options to be passed on (by() is also allowed)
*** CATCutoff    = max number of categories/levels to classify as categorical; if fails --> classified as continuous
*** MINCsize     = minium cell size required for item to be included in analysis; if fails --> classified as rare
*** MERGOptions  = merge options to be passed on to merge upon merging the imputed data with the original data	
*** MODel        = user controls the imputation model used for a specific scale
*** SAVEmidata   = save the mi data; valid path and filename required
*** PRINTmodel   = prints the imputation model
*** suspend      = 
*** CONDImputed  = conditional imputation as per Stata's manual (endogenous vars) 
*** CONDComplete = imputation subject to conditioning on a complete variable
*D** SCALEInclude = 
*D** SCALEOmit    = 
*** FULLscales   = by-passing Plumpton
*** debug        = undocumented option: interrupts execution after reshape
*** USELabels    = use labels of item/scale if exist to classify items 	(not in use)

********************************************************************************
*** Program
********************************************************************************

*** We start with data in long form
capture program drop pchained
program define pchained, eclass

	syntax anything [if] [in] [pw aw fw iw/], Ivar(varlist) Timevar(varname) /// 
						      [CONTinous(namelist) ///
							   MIOptions(string asis) CATCutoff(integer 10) ///
						       MINCsize(integer 0) MERGOptions(string asis) ///
							   MODel(string asis) CONDImputed(string asis) ///
							   CONDComplete(string asis) COMMONcov(string asis) ///
							   SAVEmidata(string) PRINTmodel suspend debug] // USELABels

	*** Warn user they need moremata
	no di in gr "Warning: this program requires package moremata."
	qui {
		
		************************************************************************
		**** Specification of defaults
		
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
		
		*** Default mergoptions
		if "`mergoptions'" == "" {
			local mergoptions ", keep(match)"
		}
		else {
			local mergoptions ", `mergoptions'"
		}
	
	
		************************************************************************
		**** Data and sample
	
		*** Save original data
		tempfile originaldata
		save "`originaldata'", replace
		
		*** Constrain sample to user-specified
		marksample touse
		drop if !`touse' 
		
		
		************************************************************************
		**** Preliminary parsing to identify and collect inputs; checks
		
		*** Parse the anything string
		_parseAnything "`anything'"
		
		*** Preliminary scan of models to identify types of inputs		
		local miScale ""       // scales
		local miSadv  ""       // stand-alone variables
		local miModelCovs ""   // covariates included in the models
		
		local i = 1
		while `"`s(model`i')'"' ~= "" {
			*** Parse the model
			_parseModels "`s(model`i')'"
			
			*** Check if dvar is a scale or sadv
			local remainingOpts "`s(remaningOpts)'"
			if `:list posof "scale" in remainingOpts' ~= 0 {
				local miScale "`=trim("`miScale' `s(depv)'")'"
			}
			else {
				local miSadv "`=trim("`miSadv' `s(depv)'")'"
			}
			local miModelCovs "`=trim(stritrim("`miModelCovs' `s(covs)'"))'"
			local ++i
		}
		
		*** Model inputs are identified
		noi di "Scales: `miScale'"
		noi di "SADvariables: `miSadv'"
		noi di "Model covariates: `miModelCovs'"
	
	
		*** Covariate collection
		
		*** Collect all covariates and check for duplication and miss-specification
		fvunab allCovs: `commoncov' 
		local allCovs "`allCovs' `miModelCovs'"
		local allCovs: list uniq allCovs
		
		noi di "Full set of covariates: `allCovs'"

		*** <><><> Check for duplication (factor vs non-factor)
		fvrevar `allCovs', list
		local covarCheck "`r(varlist)'"
		
		local listUnique: list uniq covarCheck  // 
		noi di "List of unique covariates: `listUnique'"
		local myTest: list covarCheck - listUnique

		if "`myTest'" ~= "" {
			fvrevar `myTest', list
			local myTest "`r(varlist)'"
			noi di in r "Variables `myTest' specified inconsistently across equations"
			exit 489
		}
		
	
		************************************************************************
		**** Pre-processing for reshape

		*** >>>  SCALES
		
		*** Collecting and renaming items across all scales for reshape
		local scaleItemsRS ""
		foreach scale of local miScale {  // loop over scales
			capture unab myItems: `scale'*
			
			*** Check whether elements of myItems are variables
			if _rc {
				di in r "Stub `scale' is not associated with a scale (does not identify a set of variables in the dataset)"
				exit 111
			}
			
			*** Rename items in dataset and in the locals to facilitate reshape
			foreach item of local myItems {
				local scaleItemsRS "`scaleItemsRS' `item'_`timevar'"
				ren `item' `item'_`timevar'
			}
						
			**** Feed in variable type information
			local isCont: list scale in continous   // check if user wants the items to be treated as continuous
			if `isCont' == 1 { 
				local userDefCont "`userDefCont' `scale'"
			} 
		}
		**** Scale items have been renames in the dataset and in the reshape local
		
		*** >>> SADV	
		
		*** Collecting and renaming sadv's for reshape
		local miSadvRS ""
		foreach var of local miSadv {
			local miSadvRS "`miSadvRS' `var'_`timevar'"
			ren `var' `var'_`timevar'
		}
		
		*** >>> COVARIATES

		if "`allCovs'" ~= "" {  // if covariates are specified
			*** Extract covariate names from covariate list (which may be fvvarlist)
			fvrevar `allCovs', list
			local covars "`r(varlist)'"
			
			*** Obtain a list of unique names
			local covars: list uniq covars  // use for checking conditions
		
			*** Separate time invariant from time variant covariates
			local covInvar ""
			local covVar ""
			foreach covar of local covars {
				tempvar mytest
				sort `ivar' `timevar'
				bys `ivar': egen `mytest' = mean(`covar')
				
				capture assert `mytest' == `covar'
				if _rc {
					local covVar "`covVar' `covar'"
				}
				else {
					local covInvar "`covInvar' `covar'"
				}
			}	
		
			*** Report covariates
			noi di _n in y "********************************************************"
			noi di in y "Covariates: "
			noi di in y "    Time-invariant: `covInvar'"
			noi di in y "    Time-variant  : `covVar'"
			noi di "********************************************************"	
			
			
			*** Rename time variant covariates to facilitate reshape
			local covVarRS ""
			foreach cv of local covVar {
				ren `cv' `cv'_`timevar'
				local covVarRS "`covVarRS' `cv'_`timevar'"
			}
		
			*** Create a list for all covariates (with new names) for reshape
			local covarsRS "`covVarRS' `covInvar'"
			
		}
		else {  // if no covariates
			noi di _n in y "********************************************************"
			noi di in y "No covariates included in the imputation models "
			noi di "********************************************************"		
		}
		
		* noi di "Covariates for reshape: `covarsRS'"
		*** All covariates are ready for reshape
		
		
		*** >>> TIMEVAR
		
		*** Collect the level of timevar
		levelsof `timevar', local(timelevs)

		
		************************************************************************
		*** <><><> Checking validity of inputs for imputation subject to conditions 
		
		*** Checking condimputed() for invalid inputs 
		_isInModel `"`condimputed'"' "cImp" "`miScale'" "`miSadv'"
		
		*** Checking condcomplete() for invalid inputs
		sreturn clear
		_isInModel `"`condcomplete'"' "" "`miScale'" "`miSadv'" "`covars'"

			
		
		************************************************************************
		*** Subsetting the data to only variables used in the model

		*** Keep only variables of interest	
		keep `scaleItemsRS' `miSadvRS' `covarsRS' `ivar' `timevar' `byGroup' `exp'
		
		
		************************************************************************
		*** Reshaping the data
			
		*** Reshape to wide
		noi di _n in y "Reshaping to wide..."
		reshape wide `scaleItemsRS' `miSadvRS' `covVarRS', i(`ivar') j(`timevar')
		order _all, alpha  // useful for debugging
		*** We are imputing with data in WIDE form. 
		
				*** Undocumented feature, stop execution to debug after reshaping
		if ("`debug'" ~= "") {
			noi di "Debugging suspension requested."
			exit
		}
			

		exit

		
		
		
		
		
		
		
		**** Parse MODel (get model and options)
		if `"`model'"' ~= `""' {
			* noi di `"`model'"'
			_parse_model `"`model'"' "_model"  // gives s(`scale'_model)
		}
		*noi sreturn list
		
		*** Specify temp names for scalars and matrices
		tempname vals freqs pCats nCats   
		
		foreach scale of local namelist {  // loop over scales
			
			*** Check item type as well as constant items and rare categories
		
			local bin  "" // binary items
			local cat  "" // multiple category items
			local cont "" // continuous items
			
			local finalScale ""   // admitted items
			local constant ""        // constant items
			local rare ""            // items with rare categories 
			local cuscont ""		// Items designated as continous by user

			*** Collect all items of the scale
			unab myscale: `scale'*
			
			*** User assignment to continuous
			local userOverride: list scale in userDefCont // is scale in user-defined?
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
					else if (_rc == 134) {
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
				*** Automatic assignment to all various types (may also have to look at the labels if exist!)
				foreach item of local myscale {  //iterate over items of scales
					
					*** Retrieve label info if it exists ***
					local labname: value label `item'
					local labs ""
					if "`labname'" ~= "" {
						mata: values = .; text = ""
						mata: st_vlload("`labname'", values, text); _transpose(values)
						* mata: st_local("labs", invtokens(strofreal(values)))
						mata: st_local("nCatsLab", strofreal(cols(values)))
					}
					
					*** Observed values
					capture tab `item', matrow(`vals') matcell(`freqs')
					if (_rc == 0) {  // if does not break tab
						mata: st_numscalar("`nCats'", rows(st_matrix("`vals'"))) // number of categories

/*
						*** Giving labels precedence
						if ("`uselabels'" ~= "") {  // user override for categories; use labels
							if ("`nCatsLab'" ~= "") {
								scalar `nCats' = `nCatsLab'
							}
							else {
								di in r "No label exists for this item/scale."
								exit 1000
							}
					}
*/
						* noi di `nCats'
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
			} // end of else in userOverride
			
			* noi di "`finalScale'"
			
			*** Report results by scale
			noi di _n "********************************************************" _n ///
			"Summary of pre-imputation checks for scale `scale'*" _n  ///
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
			local remaining = trim(subinstr("`namelist'", "`scale'","", .))
			* noi di "`remaining'"
			
			local include_items ""
		
			if "`remaining'" ~= "" {   // if there are other scales
				if "`fullscales'" == "" { //  if we have not requested full/complete scales
					*** Compute aggregates by the levels of timevar
					foreach remscale of local remaining {
						unab myitems: `remscale'*
						foreach tlev of local timelevs {
							local taggregs ""
							foreach item of local myitems {	
								if regexm("`item'", "^`remscale'[a-zA-Z0-9_]*_`timevar'`tlev'$") {
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
				} // end of fullscales
				else {
					foreach remscale of local remaining {
						unab myitems: `remscale'*
						local include_items "`include_items' `myitems'"
					}
				}	
			} // end of remaining
			
			*** Include variables specified in SCALEIncluded
			if `"`scaleinclude'"' ~= "" {
				 * noi di `"`scaleinclude'"'
				_parse_scaleCustomize `"`scaleinclude'"' "incl" "`timevar'" "`cov_var'" "`cov_invar'"
			}
			local include_items "`include_items' `s(`scale'_cov_incl)'"
			
			noi di "`include_items'"
			
			*** adding any stand-alone variables to include
			if ("`addsadepvars'" ~= "") {  // if stand-alone dep variable models are specified
				foreach sadVar of local addsadepvars {
					unab sadVars: `sadVar'_`timevar'*   // take all time periods of the var
					foreach svar of local sadVars {
						local include_items "`include_items' (`svar')" 
					}
				}
			}
			* no di "`finalScale'"
			* no di "`include_items'"
			* exit
						
			*** write out the imputation models for the scales
			foreach depvar of local finalScale {
				local rhs_imputed = trim(subinstr("`finalScale'", "`depvar'", "", .))
				
				***  incorporate conditional imputation into the imputation model, if provided
				noi _condimputation `"`condimputed'"' "`scale'" "`depvar'" "`timevar'" "`miDepVarsOriginal'" "`cov_invar'"
				local condImp "`s(condImp)'"

				***  incorporate condition on exogenous/complete regressor into the imputation model, if provided
				noi _condimputation `"`condcomplete'"' "`scale'" "`depvar'" "`timevar'" "" "`cov_invar'" "`cov_var'" "Comp"
				local condComp "`s(condComp)'"

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
							
				local userModel `"`s(`scale'_model)'"'
				if (`"`userModel'"' == "") {
					if `: list depvar in bin' {
						local mymodel "`mymodel' (logit `condComp', noimputed augment include(`include_items' `rhs_imputed_pr') `condImp') `depvar' "
					}
					else if `: list depvar in cat' {
						local mymodel "`mymodel' (ologit `condComp', noimputed augment include(`include_items' `rhs_imputed_pr') `condImp') `depvar' "
					}
					else {
						local mymodel "`mymodel' (reg `condComp', noimputed include(`include_items' `rhs_imputed_pr') `condImp') `depvar' "				
					}
				}
				else {
					if (regexm("`userModel'", ",[ ]*") == 0) {
						local userModel "`userModel' `condComp', "
					}
					else {
						local userModel "`=subinstr("`userModel'", ",", " `condComp',",1)'"
					}
					local mymodel "`mymodel' (`userModel' noimputed include(`include_items' `rhs_imputed_pr') `condImp') `depvar' "
				}
			} // end of loop over finalScale
		}   // end of loop over scales

		
		*** If covariates and weights are (not) provided
		local covars_wide ""
		if "`scalecovars'" ~= "" {
			*** Build list of covariates in wide format
			foreach cov of local scalecovars {
				fvunab mycov: `cov'*	
				foreach cvar of local mycov {
					if !`:list cvar in covars_wide' {
						local covars_wide "`covars_wide' `mycov'"
					}
				}
			}
			
			* noi di "`scalecovars'"
			* noi di "`covars_wide'"
			
			*** write out the exogenous vars and mi options
			
			**** By Zitong: adding sampling weight. The syntax is a little bit lengthy but more clear 
			if "`weight'" ~= "" {
				local model_endpart "= `covars_wide' [`weight'=`exp'], `mioptions'"  // covars weight and mioptions
			}
			else {
				local model_endpart "= `covars_wide', `mioptions'"	// covars , and mioptions			
			}
		}
		else {
			if "`weight'" ~= "" {
				local model_endpart "[`weight'=`exp'], `mioptions'"  // weight, and mioptions
			}
			else {
				local model_endpart ", `mioptions'" // Just mioptions			
			} 
		}	
				
		*** write out the imputation models for the miDepVars
		* noi di "`miDepVars'"
		* exit
		
		if ("`miDepVars'" ~= "") {
			noi di _n "********************************************************" _n ///
			"Stand-alone dependent variables included in the imputation model:" _n ///
			"      `miDepVarsOriginal'" 
			noi di "********************************************************" _n ///
			
			_input_parser "`anything'"
			
			local iterModels = 1
			while `"`s(ovar`iterModels')'"' ~= "" {
				*** parse the syntax of the model
				_parse_ovar_model "`s(ovar`iterModels')'"
				* noi sreturn list
				local miCovVar "`s(covs)'"     // covariates
				local miIncVars "`s(includeVars)'"   // other depVars/means/sums of scales
				local miOmitVars "`s(omitVars)'"       // omit
				local miOpts "`s(remaningOpts)'"         // other options
				
				* noi di "`miCovVar'"
				* noi di "`miIncVars'"
				
				*** retrieve the user supplied model
				_parse_model `"`model'"' "_model"
				local userModel `"`s(`s(depv)'_model)'"'
				/*
				if (regexm("`userModel'", ",[ ]*") == 0) {
						local userModel "`userModel', "
				}	
				*/
				*** collect all periods of the dependent variable
				unab miDepVar: `s(depv)'_`timevar'*
				
				
				* noi di "`miDepVar'"
				* noi di "`miIncVars'"
				
				*** collect all covariates for all periods
				local miCovWide ""
				if "`miCovVar'" ~= "" {
					foreach var of local miCovVar {
						fvunab placeholder: `var'*					
						foreach myVar of local placeholder {
							if regexm("`myVar'", "`var'(_`timevar'[0-9]+)?$") {
								local miCovWide "`miCovWide' `myVar'"
							}
						}
					}	
				}
				
				* noi di "`miCovWide'"
				
				* noi di "`miCovVar'"
				* noi di "`miOmitVars'"
				* noi di "`covars_wide'"

				*** collect all omited variables
				if "`miOmitVars'" ~= "" {
					local miOmit ""
					
					*** Build list of omitted vars
					local myOmitList ""
					* noi di "`miOmitVars'"
					
					foreach var of local miOmitVars {
						*** Check if asked to omit a variable not present in the model
						capture fvunab fullList: `var'*
						if _rc {
							noi di in r "Variable(s) `var' cannot be omited because it is not included in the model"
							error 486
						}
						local myOmitList "`myOmitList' `fullList'"
					}
					
					*** Compare each covariate in scalecovs to the omitted list
					foreach cvar of local covars_wide {
						fvrevar `cvar', list
						local myCovar "`r(varlist)'"
						if `:list myCovar in myOmitList' {
							local miOmit "`miOmit' `cvar'"
						}
					}
				}

				* noi di "`miOmit'"
				* noi di "`miCovWide'"
				
				*** if miCovWide are specified, all scalecovs are omitted
				if ("`miCovWide'" ~= "" ) {
					*compare_lists "`covars_wide'" "`miCovWide'"
					*local omit "`s(differences)'"
					loca intersection: list covars_wide & miCovWide
					local omit: list covars_wide - intersection
					*local omit "`:list covars_wide - miCovWide'"
				}
				else if ("`miOmit'" ~= "") { // omited vars should be from the scalecovs list
					local omit "`miOmit'"
				}
				* noi di "`miCovWide'" 
				*noi di "`covars_wide'"
				
				local meanList ""
				local sumList ""
				local oDepVarList ""
				local oDepVars ""
				local condImp ""
				local condComp "" 
				local oDepvarListFiltered "" 
				
				*** Build the model for the stand-alone var at every timepoint
				foreach var of local miDepVar {
					
					***  incorporate conditional imputation into the imputation model, if provided
					noi _condimputation `"`condimputed'"' "" "`var'" "`timevar'" "`miDepVarsOriginal'" "`cov_invar'"
					* noi sreturn list
					local condImp "`s(condImp)'"
					* noi di "`condImp'"

					***  incorporate condition on exogenous/complete regressor into the imputation model, if provided
					noi _condimputation `"`condcomplete'"' "" "`var'" "`timevar'" "" "`cov_invar'" "`cov_var'" "Comp"
					local condComp "`s(condComp)'"
					* noi di "`condComp'"
					
	
					
					*** create the imputed variable lists in include
					if "`miIncVars'" ~= "" {					
						*** include implies noimputed!!!
						if !regexm("`miOpts'", "noimputed") {
							local miOpts "`miOpts' noimputed"
						}
					
						*** retrieve time period of depVar
						if regexm("`var'","_`timevar'([0-9]+)$") {
							local timePeriod `=regexs(1)'
						}					
						*** mean score?
						if regexm("`miIncVars'","mean\(([a-zA-Z0-9_ ]+)\)") {
							local meanList `=regexs(1)'
							local meanRemove `=regexs(0)'
							// to replace from period-specific to all periods, replace timePeriod with timelevs
							_meanSumInclude "`meanList'" "mean" "`timevar'" "`timelevs'"
							local meanList "`s(include_items)'"
						}
						*** sum score?
						if regexm("`miIncVars'","sum\(([a-zA-Z0-9_ ]+)\)") {
							local sumList `=regexs(1)'
							local sumRemove `=regexs(0)'
							// to replace from period-specific to all periods, replace timePeriod with timelevs
							_meanSumInclude "`sumList'" "sum" "`timevar'" "`timelevs'"
							local sumList "`s(include_items)'"
						}
						
						* noi di "`var': `oDepVarList'"
						
						*** other depVar?
						local oDepVarList = subinstr("`miIncVars'","`meanRemove'", "", .)
						local oDepVarList = subinstr("`oDepVarList'","`sumRemove'", "", .)
						local oDepVarList = stritrim("`oDepVarList'")
						
						* noi di "`var': `oDepVarList'"
						
						
						*** Check for non-imputed variables and include only those vars that are in miDepVarsOriginal but not
						*** the dep var itself
						
						if "`oDepVarList'" ~= "" {
							foreach ovar of local oDepVarList {
								unab fullList: `ovar'*
								foreach fvar of local fullList {
									if regexm("`fvar'", "(.+)_`timevar'[0-9]+$") {
										local varStub "`=regexs(1)'" // y1
										if `:list varStub in miDepVarsOriginal' { // check if var in depvarlist
											if regexm("`var'", "(.+)_`timevar'[0-9]+$") {
												if "`=regexs(1)'" != "`varStub'" {
													local oDepvarListFiltered "`oDepvarListFiltered' `fvar'"
												}
											}
										}					
									}
								}
								if `:list ovar in namelist' {  // check if var in list of scales
									unab fullList: `ovar'*
									local oDepvarListFiltered "`oDepvarListFiltered' `fullList'"
								}
							}	
						}
					}	
					*noi di "`var': `oDepVarList'"
					*noi di "`var': `oDepvarListFiltered'"
					
					*** extract the model from user input
					if (regexm("`userModel'", ",[ ]*") == 0) {
						local userModelVar "`userModel' `condComp',"
					}
					else {
						local userModelVar "`=subinstr("`userModel'", ",", " `condComp',", 1)'"
					}
					* noi di "`userModelVar'"
					
					*** retrieve the list of omited vars
					if "`omit'" ~= "" {
						local omitOpt "omit(`omit')"
					}
					
					*** collect	remaining depVar timepoints
					local depVarRemaining: list miDepVar - var
					local updateRemaining ""
					foreach myVar of local depVarRemaining {
						local updateRemaining "`updateRemaining' (`myVar')"
					}
					
					*** collect oDepVars
					* noi di "`oDepVarList'"
					* noi di "`oDepvarListFiltered'"
					
					if "`oDepvarListFiltered'" ~= "" {  // this is from include()
						foreach mydVar of local oDepvarListFiltered {
							local oDepVars "`oDepVars' (`mydVar')"
						}
					}
					* noi di "`updateRemaining'"
					* noi di "`oDepVars'"
					* noi di "`miOpts'"
					* noi di "`includeOpt'"
					* noi di "`var': `miCovWide'"
					
					*** Review this!!
					if regexm("`miOpts'", "noimputed") {
						*** create the list of expressions for include
						local includeOpt "include(`updateRemaining' `miCovWide' `meanList' `sumList' `oDepVars')"
					}
					else {
						local includeOpt "include(`miCovWide' `meanList' `sumList')"
					}
					
					*** write the variable model out
					local mymodel "`mymodel' (`userModelVar' `miOpts' `includeOpt' `omitOpt' `condImp') `var' "
					* noi di "`mymodel'"
					*exit
					
				}
				local ++iterModels
				_input_parser "`anything'"
			}
		}
		* noi di "`mymodel'"
		* exit
		

		*** Write out the complete model

		local model_full "`mymodel' `model_endpart'"
		
		*** Print the full model (useful for debigging)
		if "`printmodel'" ~= "" {
			
			noi di _n in y "Printing the full imputation model..."
			noi di _n "`model_full'" 
			
			if "`suspend'" ~= "" {
				noi di _n in y "Suspending -pchained-... done"	
				exit
			}
		}
			
		*** mi set the data
		mi set flong
		
		*** register all imputed variables
		foreach scale of local namelist {
			mi register imputed `scale'*
		}
		foreach depVar of local miDepVars {
			mi register imputed `depVar'*
		}
		
		
		*** mi impute chained
		noi di _n in y "Performing multiple imputation..."
		
		noi mi impute chained `model_full'

		*** reshape to long
		mi reshape long `allitemsrs' `miDepVars' `cov_var_rs', i(`ivar') j(`timevar')
		
		*** rename vars to original names
		noi di "`miDepVars'"
		noi di "`allitemsrs' `miDepVars'"
		foreach var of varlist `allitemsrs' `miDepVars' `cov_var_rs' {
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
		noi mi merge m:1 `ivar' `timevar' using "`originaldata'" `mergoptions'
		*mi merge 1:m `ivar' `timevar' using "`savemidata'", keep(match)
		mi update
		
		noi di _n in y "Imputation finished successfully."

		
		*** Return useful macros
		ereturn local constantItems "`constant'"
		ereturn local binaryItems  "`bin'"
		ereturn local multiCategoryItems "`cat'"
		ereturn local autoContinuousItems "`cont'"
		ereturn local userContinuousItems "`cuscont'"
		ereturn local rareItems "`rare'"
		ereturn local imputedItems "`finalScale'"
		
	} // end of quietly
	
end


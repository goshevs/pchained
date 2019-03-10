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
		local miScale ""           // scales
		local miSadv  ""           // stand-alone variables
		local miModelCovs ""       // covariates included in the models
		local allOutcomes ""    
		local isContScale ""       // has user specified that scale should be continuous?
		local commonModelCovs ""   // covariates present in all models 
		
		local i = 1
		while `"`s(model`i')'"' ~= "" {
			*** Parse the model
			_parseModels "`s(model`i')'"
			
			*** Check if dvar is a scale or sadv
			local remainingOpts "`s(remaningOpts)'"
			if `:list posof "scale" in remainingOpts' ~= 0 {
				local miScale "`=trim("`miScale' `s(depv)'")'"
				if regexm("`remainingOpts'", "cont|continuous") {
					local isContScale "`=trim("`isContScale' `s(depv)'")'"
				}
			}
			else {
				local miSadv "`=trim("`miSadv' `s(depv)'")'"
			}
			
			local miModelCovs "`=trim(stritrim("`miModelCovs' `s(covs)'"))'"
			local ++i
		}
		
		*** Combine outcomes
		local allOutcomes "`miScale' `miSadv'"
		
		*** Model inputs are identified
		noi di "All outcomes: `allOutcomes'"
		noi di "Scales: `miScale'"
		noi di "Continuous scales: `isContScale'"	
		noi di "SADvariables: `miSadv'"
		noi di "Model covariates: `miModelCovs'"	
		
		*** Covariate collection
		*** Collect all covariates and check for duplication and miss-specification
		fvunab commonCov: `commoncov' 
		local allCovs "`commonCov' `miModelCovs'"
		local allCovs: list uniq allCovs
		
		noi di "Common covariates: `commonCov'"
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
		
		
		*** +++ Undocumented feature: stop execution to debug after reshaping
		if ("`debug'" ~= "") {
			noi di "Debugging suspension requested."
			exit
		}
		
		************************************************************************
		*** Parsing option MODel
		
		/*
		if `"`model'"' ~= `""' {
			* noi di `"`model'"'
			_parseMODel `"`model'"' "_model"  // gives s(`scale'_model)
		}
		* noi sreturn list
		*/
		
		
		noi di _n "********************************************************" _n ///
			"Stand-alone dependent variables included in the imputation model:" _n ///
			"      `miSadv'" 
			noi di "********************************************************" _n ///
		
		
		
		************************************************************************
		*** Parsing anything and build the models
	
		_parseAnything "`anything'"
		* noi sreturn list
		
		local iterModels = 1
		while `"`s(model`iterModels')'"' ~= "" {
			*** Parse the syntax of the model
			_parseModels "`s(model`iterModels')'"
			* noi sreturn list
			local miCovVar "`s(covs)'"            // collect covariates
			local miIncVars "`s(includeVars)'"    // collect other depVars/means/sums of scales
			local miOmitVars "`s(omitVars)'"      // collect omit
			local miOpts "`s(remaningOpts)'"      // collect other options
			
			* noi di "`miCovVar'"
			* noi di "`miIncVars'"
			
			*** --- Retrieve OUTCOME
			
			local depVarMod "`s(depv)'"
			
			*** --- Retrieve MODEL if in MODel option
			
			_parseMODel `"`model'"' "_model"
			local userModel `"`s(`depVarMod'_model)'"'

			* noi di "`miDepVar'"
			* noi di "`miIncVars'"
			
			*** --- Collect COVARIATES from all time-periods
			
			local miCovList ""
			if "`miCovVar'" ~= "" {
				foreach var of local miCovVar {
					fvunab placeholder: `var'*					
					foreach myVar of local placeholder {
						if regexm("`myVar'", "`var'(_`timevar'[0-9]+)?$") {
							local miCovList "`miCovList' `myVar'"
						}
					}
				}	
			}

			* noi di "`miCovVar'"    // covariates as enter into the syntax
			* noi di "`miCovList'"   // covariates as enter the model
			
			
			*** --- Collect OMITTED variables
			
			local miOmitList ""
			if "`miOmitVars'" ~= "" {
				*** Build list of omitted vars
				foreach var of local miOmitVars {
					*** Check if asked to omit a variable not present in the model
					capture fvunab fullList: `var'*
					if _rc {
						noi di in r "Variable(s) `var' cannot be omited because it is not included in the model"
						error 486
					}
					local miOmitList "`miOmitList' `fullList'"
				}
			}
			* noi di "`miOmitVars'"   // omited variables as enter into the syntax
			* noi di "`miOmitList'"   // omited variables as enter the model
			
	
			
			*** --- BUILDING THE SYNTAX ---

			local dVarList ""     // list of all scale items/variables across time periods

			local meanList ""     // list of means
			local sumList ""      // list of sums
			local rIncVarList ""  // list of other included variables
			local rDepVars ""     // list of other included vars as required by mi impute chained
			
			local condImp ""      // condition for conditional imputation
			local condComp ""     // condition for imputation on exogenous/complete predictor

			local scale ""        // only populated if var is a scale
				
				
			if `:list depVarMod in miScale' {            // building the list of items of the scale at all time points
				*** Categorize the items of the scale
				noi _scaleItemCategorization "`depVarMod'" "`isContScale'" "`catcutoff'" "`mincsize'"
				local dVarList "`s(finalScale)'"  // building the items of the scale (for all time points)
				local itemsBin "`s(bin)'"
				local itemsMCat "`s(mCat)'"
				local itemsCon "`s(contUI)'"
				
				local scale "`depVarMod'"
			}
			else {  // if outcome is a sadv
				unab dVarList: `depVarMod'*             // building the list of sadv at all time points
			}
			
			*** Outcomes in all time periods
			* noi di "`dVarList'"
		
			foreach dVar of local dVarList {
				 
				*** +++ Incorporate imputation subject to conditions, if provided
				*** Conditional imputation
				_imputationS2Cond `"`condimputed'"' "`scale'" "`dVar'" "`timevar'" "`miSadv'" "`covInvar'"
				local condImp "`s(condImp)'"
				
				*** Imputation subject to an exogenous/complete regressor
				_imputationS2Cond `"`condcomplete'"' "`scale'" "`dVar'" "`timevar'" "" "`covInvar'" "`covVar'" "Comp"
				local condComp "`s(condComp)'"
				
				* noi di "Conditional imputation: `condImp'"
				* noi di "Condition: `condComp'"
			
				*** Create the INCLUDE variable lists
				if "`miIncVars'" ~= "" {					

					if !regexm("`miOpts'", "noimputed") { // include() implies nonimputed 
						local miOpts "`miOpts' noimputed"
					}
				
					*** Retrieve time period of dVar
					if regexm("`dVar'","_`timevar'([0-9]+)$") {
						local timePeriod `=regexs(1)'
					}					
					*** Create --MEAN-- scores
					if regexm("`miIncVars'","mean\(([a-zA-Z0-9_ ]+)\)") {
						local meanList `=regexs(1)'
						local meanRemove `=regexs(0)'
						// ()()() option: to replace from period-specific to all periods, replace timePeriod with timelevs
						_meanSumInclude "`meanList'" "mean" "`timevar'" "`timelevs'"
						local meanList "`s(include_items)'"
					}
					*** Create --SUM-- scores
					if regexm("`miIncVars'","sum\(([a-zA-Z0-9_ ]+)\)") {
						local sumList `=regexs(1)'
						local sumRemove `=regexs(0)'
						// ()()() option: to replace from period-specific to all periods, replace timePeriod with timelevs
						_meanSumInclude "`sumList'" "sum" "`timevar'" "`timelevs'"
						local sumList "`s(include_items)'"
					}
					
					*** Remaining variables in include
					
					local rIncVarList = subinstr("`miIncVars'","`meanRemove'", "", .)
					local rIncVarList = subinstr("`rIncVarList'","`sumRemove'", "", .)
					local rIncVarList = stritrim("`rIncVarList'")
					
					* noi di "`depVarMod': `rIncVarList'"
					
					*** <><><> Check for non-imputed variables and include only those vars that are in allOutcomes
					*** except for the dep var itself
					
					local rIncVarListUpdated ""   // list of valid included variables
					if "`rIncVarList'" ~= "" {
						foreach rVar of local rIncVarList {
							*** Check if there is a wildcard in rVar
							local wCard = strpos("`rVar'", "*")
							if `wCard' ~= 0 {  // if there is a wildcard
								local preFix = substr("`rVar'", 1, `=`wCard' -1') // get the prefix
								// go through the list of allOutcomes to see which they are
								foreach out of local allOutcomes {
									if "`=substr("`out'", 1, `=`wCard' -1')'" == "`preFix'" {
										local rIncVarListUpdated "`rIncVarListUpdated' `out'"
									}
								}
							}
							else {  // there is no wildcard
								local rIncVarListUpdated "`rIncVarListUpdated' `rVar'"
							}
						}
						local rIncVarListUpdated: list rIncVarListUpdated - depVarMod // remove current outcome from list
					
						*** Create the final complete list of included variables
						local rIncVarListFinal ""
						foreach finalVar of local rIncVarListUpdated {
							unab collection: `finalVar'*
							local rIncVarListFinal "`rIncVarListFinal' `collection'"
						}
						* noi di "`depVarMod':: `rIncVarListFinal'"
					}
				}  // end of INCLUDE
				
				*** Write out the include varlist as required by -mi impute chained-
				local rDepVars ""
				if "`rIncVarListFinal'" ~= "" { 
					foreach mydVar of local rIncVarListFinal {
						local rDepVars "`rDepVars' (`mydVar')"
					}
				}
						
				*** Write out the INCLUDE option as it should enter the model
				local includeOpt "`miCovWide' `meanList' `sumList'"
				if regexm("`miOpts'", "noimputed") {
					*** If no imputed variables will be included, add remaining outcomes and dependent vars
					local includeOpt "`dVarComplement' `rDepVars' `includeOpt'"
				}
				local includeOpt "include(`includeOpt')"
								
								
				*** Remove options belonging to -pchained- from MIOPTS
				if regexm("`miOpts'", "[ ]*(scale)[ ]*") {
					local miOpts = subinstr("`miOpts'", "`=regexs(1)'", "", .)
				}
				if regexm("`miOpts'", "[ ]*(cont|continous)[ ]*") {
					local miOpts = subinstr("`miOpts'", "`=regexs(1)'", "", .)
				}
				local miOpts = stritrim(strtrim("`miOpts'"))
				
				
				*** Develop MODEL specification and incorporate conditioning
				
				if (`"`userModel'"' == "") {   // if model is not provided
					if ("`scale'" ~= "" ) {    // if scale item
						if `: list dVar in itemsBin' {    // binary items
							local userModel "logit"
						}
						else if `: list dVar in itemsMCat' {  // items with more than two categories
							local userModel "ologit"
						}
						else {   // continuous items
							local userModel "regress"				
						}
					}
					else {   // if not a scale item
						noi di in r "Provide an input in option model() for `depVarMod'"
						exit 489
					}
				}
				
				if (regexm("`userModel'", ",[ ]*") == 0) {
					local userModelVar "`userModel' `condComp',"
				}
				else {
					local userModelVar "`=subinstr("`userModel'", ",", " `condComp',", 1)'"
				}
				
				*** Set up the OMIT option as it should enter the model
				local omitOpt ""
				if "`miOmitList'" ~= "" {
					local omitOpt "omit(`miOmitList')"
				}
				
				*** Collect all other but current timepoints of dVar	
				local dVarRemaining: list dVarList - dVar
				local dVarComplement ""
				foreach myVar of local dVarRemaining {
					local dVarComplement "`dVarComplement' (`myVar')"
				}
	
				*** Write out the MODEL for dVar
				local miModel "`miModel' (`userModelVar' `miOpts' `includeOpt' `omitOpt' `condImp') `dVar' "
				* noi di _n "`miModel'"
				*exit
				
			}
			local ++iterModels
			_parseAnything "`anything'"
		}

	*noi di "`miModel'"
	
	exit
	
	
	*** Here we need to identify the common covariates in the models and check againts commonCov
    *** FIX WEIGHTING AND FIX COMMON COVARIATES
	
	
	
		/*
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
		
*/

	exit
	
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


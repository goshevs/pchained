* Plumpton with -mi impute chained-
* Authors: Simo Goshev, Zitong Liu
* Version: 1.0
*
*
*
*

*** SYNTAX ***
*** anything     = models for scales and sadv
*** Ivar         = cluster identifier (i.e. person, firm, country id)
*** Timevar      = time/wave identifier
*** MODel        = user controls the imputation model used for scales and sadv
*** COMMONcov    = list of commn covariates in scale and sadv models, supports factor variable syntax and wildcards 
*** CONDImputed  = conditional imputation as per Stata's manual (endogenous vars) 
*** CONDComplete = imputation subject to conditioning on a exogenous/complete covariate
*** CATCutoff    = max number of categories/levels to classify as categorical; if fails --> classified as continuous
*** MINCsize     = minium cell size required for item to be included in analysis; if fails --> classified as rare
*** MIOptions    = mi impute chained options to be passed on (by() is also allowed)
*** MERGOptions  = merge options to be passed on to merge upon merging the imputed data with the original data	
*** SAVEmidata   = save the mi data; valid path and filename required
*** PRINTmodel   = prints the imputation model
*** suspend      = terminate execution immediately before imputation
*** debug        = interrupts execution after reshape
*** USELabels    = use labels of item/scale if exist to classify items 	(not in use)

********************************************************************************
*** Program
********************************************************************************

*** We start with data in long form
capture program drop pchained
program define pchained, eclass

	syntax anything [if] [in] [pw aw fw iw/], Ivar(varlist) Timevar(varname) /// 
						      [MODel(string asis) COMMONcov(string asis) ///
							   CONDImputed(string asis) CONDComplete(string asis)  ///
							   CATCutoff(integer 10)  MINCsize(integer 0) ///
							   MIOptions(string asis) MERGOptions(string asis) ///
							   SAVEmidata(string) PRINTmodel suspend debug] // USELabels

	*** Warn user they need moremata
	no di in gr "Warning: this program requires package moremata."
	qui {
		
		************************************************************************
		**** Assigning default values
		
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
		local scaleNameCheck ""
		foreach scale of local miScale {  // loop over scales
			capture unab myItems: `scale'*
			
			*** Check whether elements of myItems are variables
			if _rc {
				di in r "Stub `scale' is not associated with a scale (does not identify a set of variables in the dataset)"
				exit 111
			}
			
			local scaleNameCheck "`scaleNameCheck' `myItems'"
			
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
				
		noi di _n "********************************************************" _n ///
		"Stand-alone dependent variables included in the imputation model:" _n ///
		"      `miSadv'" 
		noi di "********************************************************" _n ///
		
		
		*** >>> COVARIATES

		if "`allCovs'" ~= "" {  // if covariates are specified
			
			*** Extract covariate names from covariate list (which may be fvvarlist)
			fvrevar `allCovs', list
			local plainCovs "`r(varlist)'"
			
			*** Obtain a list of unique names
			local plainCovs: list uniq plainCovs  // use for checking conditions
		
			*** Separate time invariant from time variant covariates
			local covInvar ""
			local covVar ""
			foreach covar of local plainCovs {
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
		_isInModel `"`condcomplete'"' "" "`miScale'" "`miSadv'" "`plainCovs'"

			
		
		************************************************************************
		*** Subsetting the data to only variables used in the model

		*** <><><> Check for long names
		noi _checkVarNameLength "`scaleNameCheck' `miSadv' `plainCovs'" "`timevar'" "29"   // see manual for limits
				
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
			noi di "Debugging termination requested."
			exit
		}
		
		************************************************************************
		*** Creating the list of common covariates
		
		local miComCovList ""
		_createAllPeriods "`commoncov'" "`timevar'"
		local miComCovList "`s(expandedList)'"
		
		* noi di "`commoncov'"      // common covariates as enter into the syntax
		* noi di "`miComCovList'"   // common covariates as enter the model
		
		************************************************************************
		*** Building the model
		
		*** Store the complete list of dependent variables
		local depVarCompleteList ""
		
		_parseAnything "`anything'"
		
		local iterModels = 1
		while `"`s(model`iterModels')'"' ~= "" {

			*** Parse the syntax of the model
			_parseModels "`s(model`iterModels')'"

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
			_createAllPeriods "`miCovVar'" "`timevar'"
			local miCovList "`s(expandedList)'"
			
			* noi di "`miCovVar'"    // covariates as enter into the syntax
			* noi di "`miCovList'"   // covariates as enter the model
			
			*** --- Collect OMITTED variables
			
			local miOmitListPlain ""
			if "`miOmitVars'" ~= "" {
				*** Build list of omitted vars
				foreach var of local miOmitVars {
					*** Check if asked to omit a variable not present in the model
					capture fvunab fullList: `var'*
					if _rc {
						noi di in r "Variable(s) `var' cannot be omitted because it is not included in the model"
						error 486
					}
					local miOmitListPlain "`miOmitListPlain' `fullList'"
				}
			}

			*** Restrict the list of omitted vars to vars in the common covariates list
			local miOmitList ""
			foreach comCovar of local miComCovList {
				fvrevar `comCovar', list
				local varPlain "`r(varlist)'"
				if `:list varPlain in miOmitListPlain' {
					local miOmitList "`miOmitList' `comCovar'"
				}
			}	

			* noi di "`miOmitVars'"   // omitted variables as enter into the syntax
			* noi di "`miOmitList'"   // omitted variables as enter the model
			
	
			
			*** --- BUILDING THE INPUT ---

			local dVarList ""     // list of all scale items/variables across all time periods

			local meanList ""     // list of means
			local sumList ""      // list of sums
			local rIncVarList ""  // list of remaining included variables
			local rDepVars ""     // list of remaining included vars as required by mi impute chained
			local rIncVarListFinal "" // final list of included variables as required by mi impute chained
			
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
			
			*** Collect all dVarLists
			local depVarCompleteList "`depVarCompleteList' `dVarList'"

			*** Outcomes in all time periods
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

				
				*** Collect all but current timepoints of dVar	
				local dVarRemaining: list dVarList - dVar
				local dVarComplement ""
				foreach myVar of local dVarRemaining {
					local dVarComplement "`dVarComplement' (`myVar')"
				}
	
				*** Create the INCLUDE variable lists
				if "`miIncVars'" ~= "" {					

					if !regexm("`miOpts'", "noimputed") { // include() implies noimputed 
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
						foreach finalVar of local rIncVarListUpdated {
							unab collection: `finalVar'*
							local rIncVarListFinal "`rIncVarListFinal' `collection'"
						}
						* noi di "`depVarMod':: `rIncVarListFinal'"
					}
				}  // end of INCLUDE
				
				*** Write out the include varlist as required by -mi impute chained-
				if "`rIncVarListFinal'" ~= "" { 
					foreach mydVar of local rIncVarListFinal {
						local rDepVars "`rDepVars' (`mydVar')"
					}
				}
						
				*** Write out the INCLUDE option as it should enter the model
							
				local includeOpt "`miCovList' `meanList' `sumList'"
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
			
				*** Write out the MODEL for dVar
				local miModel "`miModel' (`userModelVar' `miOpts' `includeOpt' `omitOpt' `condImp') `dVar' "
				* noi di _n "`miModel'"
				
			}
			local ++iterModels
			_parseAnything "`anything'"
		}

		*** Write out WEIGHT syntax
		local weightOpt ""
		if "`weight'" ~= "" {
			local weightOpt "[`weight'=`exp']"
		}
		
		*** Include COMMONCOV as required by -mi impute chained-
		if "`commoncov'" ~= "" {
			local miComCovList "= `miComCovList'"
		}
		
		************************************************************************
		*** Creating the complete model syntax
		
		*** Write out the end of the model
		local miModelEndString "`miComCovList' `weightOpt', `mioptions'"
		
		*** Write out the complete model
		local modelComplete "`miModel' `miModelEndString'"
		
		
		************************************************************************
		*** Helpful debugging options		
		
		*** Printing the complete model (useful for debigging)
		if "`printmodel'" ~= "" {
			noi di _n in y "Printing the complete imputation model..."
			noi di _n "`modelComplete'" 
		}
		
		*** Suspending the execution of the program if requested by user
		if "`suspend'" ~= "" {
			noi di _n in y "Suspending -pchained-... done"	
			exit			
		}
		
		************************************************************************
		*** Implementing imputation using -mi impute chained-
		
		*** Set data as mi
		mi set flong
		
		*** Register all imputed variables
		mi register imputed `depVarCompleteList'
		
		*** Run -mi impute chained-
		noi di _n in y "Performing multiple imputation..."
		
		noi mi impute chained `modelComplete'

		
		************************************************************************
		*** Manage dataset after imputation
		
		*** Reshape to long
		mi reshape long `scaleItemsRS' `miSadvRS' `covVarRS', i(`ivar') j(`timevar')

		*** Rename variables to pre-reshape names
		foreach var of varlist `scaleItemsRS' `miSadvRS' `covVarRS' {
			if regexm("`var'", ".+(_`timevar')$") {
				ren `var' `=subinstr("`var'", "`=regexs(1)'","",.)'
			}
			else {
				noi di in r "Error renaming `var'"
				exit 486
			}
		}
		
		*** Save the data
		if "`savemidata'" ~= "" {
			noi di _n in y "Saving mi dataset..."
			save "`savemidata'", replace
		}
		
		*** Merge the midata into the original dataset
		noi di _n in y "Merging imputed dataset with original dataset..."
		noi mi merge m:1 `ivar' `timevar' using "`originaldata'" `mergoptions'
		mi update
		
		noi di _n in y "Imputation finished successfully."

	} // end of quietly
	
end


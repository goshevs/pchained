* Program -pchained- and utility functions
* Authors: Simo Goshev, Zitong Liu
* Version: 1.1
*
*
*
*

*** SYNTAX ***
*** anything     = models for scales and sadv
*** Ivar         = cluster identifier (i.e. person, firm, country id)
*** Timevar      = time/wave identifier
*** MODel        = user controls the imputation model used for scales and sadv
*** COMMONcov    = list of commn covariates in scale and sadv models, supports factor variable syntax and wild cards 
*** CONDImputed  = conditional imputation as per Stata's manual (endogenous vars) 
*** CONDComplete = imputation subject to conditioning on a exogenous/complete covariate
*** CATCutoff    = max number of categories/levels to classify as categorical; if fails --> classified as continuous
*** MINCsize     = minium cell size required for item to be included in analysis; if fails --> classified as rare
*** NAcode       = code for non-applicable observations (whenever conditioning is involved)
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
						      [COMMONcov(string asis) MODel(string asis) ///
							   CONDImputed(string asis) CONDComplete(string asis)  ///
							   CATCutoff(integer 10)  MINCsize(integer 0) ///
							   NAcode(integer -9999999) ///
							   MIOptions(string asis) MERGOptions(string asis) ///
							   SAVEmidata(string) PRINTmodel suspend debug] // USELabels

	*** Warn user they need moremata
	no di in gr "Warning: this program requires package moremata."
	qui {
		
		************************************************************************
		**** Assigning default values

		*** Default number of added datasets
		local nAdd = 5
		
		*** For parallel processing
		local prefix "`c(prefix)'"
		
		if "`prefix'" == "parallelize" {
			local nAdd = 1
		}
			
		*** Default mioptions
		if "`mioptions'" == "" {
			local mioptions "add(`nAdd')"
		} 
		else {
			// check for add() and replace with `nAdd' if parallelize
			if regexm("`mioptions'", "add\(([0-9]+)\)") {
				local myAdd "`=regexs(1)'"
				if (`myAdd' ~= 1 & "`prefix'" == "parallelize") {
					// replace add(??) with add(`nAdd') in mioptions
					local mioptions = regexr("`mioptions'", "add\([0-9]+\)", "add(`nAdd')")
					noi di _n in y "Defaulted to 1 imputation to be added"
				}			
			}	
			// check and remove rseed if included in mioptions under parallelize
			if regexm("`mioptions'", "rseed\([0-9]+\)") & "`prefix'" == "parallelize" {
				local mioptions = regexr("`mioptions'", "rseed\([0-9]+\)", "")
				noi di _n in y "Removed user provided seed of the random generator"
				
			}
			// check for by and retrieve the by varname
			if regexm("`mioptions'", "by\([a-zA-Z0-9]+\)") {
				local myby "`=regexs(0)'"
				gettoken left gr: myby, parse("(")
				gettoken left gr: gr, parse("(")
				gettoken byGroup right: gr, parse(")")
			}
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
		tempfile originalData
		save "`originalData'", replace
		
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
				_wildCardExpand "`s(depv)'"   // expand the sadv list if wild cards are present
				local sadvList "`=stritrim(strtrim("`sadvList' `s(sadvList)'"))'"
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
		noi di "SAD variables: `miSadv'"
		noi di "SAD variables expanded: `sadvList'"
		noi di "Model covariates: `miModelCovs'"	
		
		
		*** Covariate collection
		*** Collect all covariates and check for duplication and miss-specification
		if "`commoncov'" ~= "" {
			fvunab commonCov: `commoncov' 
		}
		local allCovs "`commonCov' `miModelCovs'"
		local allCovs: list uniq allCovs
		
		* noi di "Common covariates: `commonCov'"
		* noi di "Full set of covariates: `allCovs'"

		*** <><><> Check for duplication (factor vs non-factor)
		fvrevar `allCovs', list
		local covarCheck "`r(varlist)'"
		
		local listUnique: list uniq covarCheck
		* noi di "List of unique covariates: `listUnique'"
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
		
		*** >>> SADV	
		
		******* >>>> TESTING!!!
		*** SADV override
		local miSadv "`sadvList'"
		
		******* >>>>
		
		*** Collecting and renaming sadv's for reshape
		local miSadvRS ""
		foreach var of local miSadv {
			local miSadvRS "`miSadvRS' `var'_`timevar'"
			capture ren `var' `var'_`timevar'
				if _rc {
					noi di in r "Variable `var' cannot be processed. " _n ///
					"If `var' is a scale, please include option 'scale' in its equation"
					exit 111
				}
		}

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
				
			*** Rename time variant covariates to facilitate reshape
			local covVarRS ""
			foreach cv of local covVar {
				ren `cv' `cv'_`timevar'
				local covVarRS "`covVarRS' `cv'_`timevar'"
			}
		
			*** Create a list for all covariates (with new names) for reshape
			local covarsRS "`covVarRS' `covInvar'"
			
		}
		
		*** Displaying all variables
		noi _displayAllVars "`miScale'" "`miSadv'" "`covInvar'" "`covVar'"
		
		
		*** >>> TIMEVAR
		
		*** Collect the level of timevar
		levelsof `timevar', local(timelevs)

		
		************************************************************************
		*** <><><> Checking validity of inputs for imputation subject to conditions 
		
		*** Checking condimputed() for invalid inputs 
		noi _isInModel `"`condimputed'"' "cImp" "`miScale'" "`miSadv'" "" "`allOutcomes'"
		
		*** Checking condcomplete() for invalid inputs
		sreturn clear
		noi _isInModel `"`condcomplete'"' "" "`miScale'" "`miSadv'" "`plainCovs'" "`allOutcomes'"

			
		
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
		noi _createAllPeriods "`commoncov'" "`timevar'"
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
			
			*** Remove wildcard if present
			local depVarModOrig ""
			local wCard = strpos("`depVarMod'", "*")
			if `wCard' ~= 0 {
				local depVarModOrig "`depVarMod'"   // for use in _imputationS2Cond
				local depVarMod = substr("`depVarMod'", 1, `=`wCard' -1')
			}
						
			*** --- Retrieve MODEL if in MODel option
			
			_parseMODel `"`model'"' "_model"   //ys
			local userModel `"`s(`depVarMod'_model)'"'
			
			* noi di "`miDepVar'"
			* noi di "`miIncVars'"
			
			*** --- Collect COVARIATES from all time-periods
			
			local miCovList ""
			_createAllPeriods "`miCovVar'" "`timevar'"
			local miCovList "`s(expandedList)'"
			
			* noi di "`miCovVar'"    // covariates as enter into the syntax
			* noi di "COVARIATE LIST: `miCovList'"   // covariates as enter the model
			
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
			*** Exclude model covariates from miOmitList
			local commonList: list miCovList & miOmitList
			local miOmitList: list miOmitList - commonList
			
			* noi di "`depVarMod'"
			* noi di "Covars in model: `miCovVar'"
			* noi di "Covariate list: `miCovList'"
			* noi di "Common list: `commonList'"
			* noi di "Omit list: `miOmitList'"
			
			* noi di "`miOmitVars'"   // omitted variables as enter into the syntax
			* noi di "OMIT LIST: `miOmitList'"   // omitted variables as enter the model
			
			
			
			*** --- BUILDING THE INPUT ---

			local dVarList ""     // list of all scale items/variables across all time periods

			local meanList ""     // list of means
			local sumList ""      // list of sums
			local rIncVarList ""  // list of remaining included variables

			local condImp ""      // condition for conditional imputation
			local condComp ""     // condition for imputation on exogenous/complete predictor

			local scale ""        // only populated if var is a scale
			local iDotSelf = 0
			
			*** Will the remaining values of the depvar be included as categorical vars?
			if regexm("`miOpts'", "catself") {
				local iDotSelf = 1
				local miOpts "`=subinstr("`miOpts'", "catself","",.)'"
			}			
				
			if `:list depVarMod in miScale' {            // building the list of items of the scale at all time points
				*** Categorize the items of the scale
				noi _scaleItemCategorization "`depVarMod'" "`isContScale'" "`catcutoff'" "`mincsize'" "`nacode'" "`timelevs'"
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
				
				*** Define variable-specific locals
				local rIncVarListFinal ""     // final list of included variables as required by mi impute chained
				local rIncVarListUpdated ""   // list of valid included variables
				local rDepVars ""             // list of remaining included vars as required by mi impute chained
				local dVarComplement ""       // list of remaining depvars
				
				*** +++ Incorporate imputation subject to conditions, if provided
				*** Conditional imputation
				noi _imputationS2Cond `"`condimputed'"' "`scale'" "`dVar'" "`timevar'" "`miSadv'" "`covInvar'" "" "`depVarModOrig'"
				local condImp "`s(condImp)'"

				*** Imputation subject to an exogenous/complete regressor
				noi _imputationS2Cond `"`condcomplete'"' "`scale'" "`dVar'" "`timevar'" "" "`covInvar'" "`covVar'" "`depVarModOrig'" "Comp"
				local condComp "`s(condComp)'"
				
				*** Collect all but current timepoints of dVar	
				if "`scale'" ~= "" {  // if var is a scale (we need all remaning items of the scale
					local dVarRemaining: list dVarList - dVar
					
					foreach myVar of local dVarRemaining {
						if `iDotSelf' {
							local dVarComplement "`dVarComplement' i.`myVar'"
						}
						else {
							local dVarComplement "`dVarComplement' (`myVar')"
						}
					}
				}
				else {  // if var is a sadv (we only need the remaning periods of the sadv)
					foreach rdVar of local dVarList {
						*** Obtain the stub of sadv
						if regexm("`dVar'", "(.+)_`timevar'[0-9]+$") {
							local dVarStub "`=regexs(1)'"
						}
						*** Obtain the stub of rdVar
						if regexm("`rdVar'", "(.+)_`timevar'[0-9]+$") {
							local rdVarStub "`=regexs(1)'" 
						}
						if "`dVarStub'" == "`rdVarStub'" {
							if `iDotSelf' {
								local dVarComplement "`dVarComplement' i.`rdVar'"
							}
							else {
								local dVarComplement "`dVarComplement' (`rdVar')"
							}
						}
					}
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

					
					if "`rIncVarList'" ~= "" {
						foreach rVar of local rIncVarList {
						
							local fVarOp ""               // contains i.
							
							*** Remove fvvarlist syntax
							if regexm("`rVar'", "(i\.)(.+)") {  // remove fvvarlist syntax
								local rVar "`=regexs(2)'"
								local fVarOp "`=regexs(1)'"
							}
							*** Check if there is a wild card in rVar
							
							local wCard = strpos("`rVar'", "*")
							if `wCard' ~= 0 {  // if there is a wild card
								local preFix = substr("`rVar'", 1, `=`wCard' -1') // get the prefix
								// go through the list of allOutcomes to see which they are
								local allOutcomesRemaining: list allOutcomes - depVarMod
								foreach out of local allOutcomesRemaining {
									if "`=substr("`out'", 1, `=`wCard' -1')'" == "`preFix'" {
										local rIncVarListUpdated "`rIncVarListUpdated' `fVarOp'`out'"
									}
								}
							}
							else {  // there is no wild card
								local rIncVarListUpdated "`rIncVarListUpdated' `fVarOp'`rVar'"
							}
						}
					}
				}  // end of INCLUDE
				
				*** Create the final complete INCLUDE list
				foreach finalVar of local rIncVarListUpdated {
					fvunab collection: `finalVar'*
					local rIncVarListFinal "`rIncVarListFinal' `collection'"
				}
				
				if "`rIncVarListFinal'" ~= "" { 
					*** <><><> Check whether duplicated vars in dVarComplement and rIncVarListFinal and remove them
					local dVarComplementPlain = subinstr("`dVarComplement'", "(","",.)
					local dVarComplementPlain = subinstr("`dVarComplementPlain'", ")","",.)
					
					fvrevar `dVarComplementPlain', list
					local dVarComplementPlain "`r(varlist)'"
					
					fvrevar `rIncVarListFinal', list
					local rIncVarListPlain "`r(varlist)'"
					
					local dups: list dVarComplementPlain & rIncVarListPlain
					
					*** Write out the rDepVars, other included dependent variables
					**** Here consider fvvarlist syntax
					foreach mydVar of local rIncVarListFinal {
						if regexm("`mydVar'", "i\.(.+)") {
							local mydVarPlain "`=regexs(1)'"
							if !`:list mydVarPlain in dups' {
								local rDepVars "`rDepVars' `mydVar'"
							}
						}
						else {
							if !`:list mydVar in dups' {
								local rDepVars "`rDepVars' (`mydVar')"
							}
						}
					}
				}
				
				*** Combine commands and write out the INCLUDE option as it should enter the model
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
		*** Creating the complete model string
		
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
		
		*** Extract original dependant variable names
		local depVars
		foreach var of local depVarCompleteList {
			if regexm("`var'", "(.+)_`timevar'.+") {
				local myNewVar "`=regexs(1)'"
				local depVars "`depVars' `myNewVar'"
			}
		}
		
		*** Register depvars in their original names as imputed
		local depVars: list uniq depVars
		mi register imputed `depVars'
		
		
		*** Save the dataset
		if "`savemidata'" ~= "" {
			noi di _n in y "Saving mi dataset..."
			save "`savemidata'", replace
		}
		
		if "`prefix'" ~= "parallelize" {
			*** Merge the midata into the original dataset
			noi di _n in y "Merging imputed dataset with original dataset..."
			noi mi merge m:1 `ivar' `timevar' using "`originalData'" `mergoptions'
			mi update
		}
		
		noi di _n in y "Imputation completed successfully."

	} // end of quietly
	
end



********************************************************************************
*** Utility functions
********************************************************************************


*** Parse the miModel syntax
capture program drop _parseAnything
program define _parseAnything, sclass

	args input_string
	
	sreturn clear

	local model_exp "([a-zA-Z0-9_]+[a-zA-Z0-9_.\* ]*[ ]?)"
	local opts_exp "(,[ ]*([a-zA-Z]+([ ]|\(([a-zA-Z0-9_.\* ]+|mean\([a-zA-Z0-9_ ]+\)|sum\([a-zA-Z0-9_ ]+\))+\)[ ]?|))+)?"
		
	local regex_model "`model_exp'`opts_exp'"

	local count = 1
	while regexm(`"`input_string'"', `"`regex_model'"') {
		local expression `= regexs(0)'
		local input_string = trim(subinstr(`"`input_string'"', `"(`expression')"', "", .))
		sreturn local model`count' `=regexs(0)'
		local ++count
		
	}
end

*** Parse the individual models in miModel
capture program drop _parseModels
program define _parseModels, sclass
	
	args model
	
	* noi di "`model'"
	
	gettoken eq opts: model, parse(",")
	local ovar `:word 1 of `eq''
	local ovar = trim("`ovar'")
	
	gettoken left covs: eq
	local covs = trim("`covs'")
	
	gettoken right opts: opts, parse(",")
	local opts = trim("`opts'")
	
	if regexm("`opts'", "omit\(([a-zA-Z0-9_.\* ]+)\)") {
		local omitOpt `=regexs(0)'
		local omitVars `=regexs(1)'
	}
	
	if regexm("`opts'", "include\((([a-zA-Z0-9_.\* ]+|mean\([a-zA-Z0-9_ ]+\)|sum\([a-zA-Z0-9_ ]+\))+)\)") {
		local includeOpt `=regexs(0)'
		local includeVars `=regexs(1)'
		* noi di "`includeVars'"
		
	}
	
	local remaningOpts = trim(subinstr("`opts'", "`includeOpt'","", .))
	local remaningOpts = trim(subinstr("`remaningOpts'", "`omitOpt'","", .))
		
	* noi di "`omitOpt'"
	* noi di "`includeOpt'"
	
	sreturn local depv "`ovar'"
	sreturn local covs "`covs'"
	sreturn local opts "`opts'"
	sreturn local includeVars "`includeVars'"
	sreturn local omitVars "`omitVars'"
	sreturn local remaningOpts "`remaningOpts'"
	
	
	* noi di "`includeVars'"
	* noi di "`ovar'"
	* noi di "`covs'"
	* noi di "`opts'"
	
end



*** Slice conditions
capture program drop _parseConditions
program define _parseConditions, sclass

	args myinput type allOutcomes
	
	local nlistex "[a-zA-Z(-0-9)_\(\)<>~=\.\&\| ]+"
	local strregex "[a-zA-Z0-9\_\*]+[ ]*=[ ]*(\'|\")`nlistex'(\'|\")"
	local myvars "([a-zA-Z0-9_\(\) ]+)([<>~=]+)([a-zA-Z(-0-9)_\(\) ]+)(\|?\&?)" // [a-zA-Z0-9_]\|\&\.\(\)]+"
	
	*noi di `"`myinput'"'
	*noi di "TEST 0"
	
	while regexm(`"`myinput'"', `"`strregex'"') {
		local scale `=regexs(0)'
		* noi di "TEST 1"
		* noi di `"`scale'"'
		local myinput = trim(subinstr(`"`myinput'"', `"`scale'"', "", .))
		gettoken sname cond: scale, parse("=")
		gettoken left cond: cond, parse("=")
		local cond = trim(`"`cond'"')
		local cond = subinstr(`"`cond'"', `"""',"",.)
		local cond = subinstr(`"`cond'"', `"'"',"",.)
		local sname = trim("`sname'")

		
		*** Remove wild cards from all string
		local sname = subinstr("`sname'", "*", "", .)
		local allOutcomes = subinstr("`allOutcomes'","*","", .)
		
		*** Trim sname
		local sname = trim("`sname'")	

		*** <><><> Check if sname in variables included in the model
		* noi di "`sname'"
		* noi di "All outcomes: `allOutcomes'"
		
		if "`allOutcomes'" ~= "" {
			if !`:list sname in allOutcomes' {
				noi di in r _n "Error in condi() or condc(): `sname' not found in the model."
				error 489
			}
		}
		
		*** parsing the if condition
		local ifL "" // left side
		local ifS "" // signs
		local ifR "" // right side
		local ifB "" // between
		*noi di "`cond'"
		
		local mycond = regexr(`"`cond'"', "^[ ]*if[ ]+", "")
		while regexm(`"`mycond'"', `"`myvars'"') {
			local ifL "`ifL' `=regexs(1)'"
			local ifR "`ifR' `=regexs(3)'"
			local ifS "`ifS' `=regexs(2)'"
			local ifB "`ifB' `=regexs(4)'"
			local mycond = subinstr(`"`mycond'"', "`=regexs(0)'", "",.)
			
		}
		
		* noi di "`cond'"
		* noi di "`ifB'"
		
		*** Post result
		sreturn local cond`type'_`sname' `cond'
		sreturn local ifL`type'_`sname' `=itrim("`ifL'")'
		sreturn local ifR`type'_`sname' `=itrim("`ifR'")'
		sreturn local ifS`type'_`sname' `"`=trim("`ifS'")'"'
		sreturn local ifB`type'_`sname' `"`=trim("`ifB'")'"'
	}
end


capture program drop _isInModel
program define _isInModel, sclass

	args condition type miScale miSadv covars allOutcomes
	
	_parseConditions `"`condition'"' "" "`allOutcomes'"
	
	foreach side in "L" "R" {
		foreach element in `:s(macros)' {
			if regexm("`element'", "^if`side'_.+") {
				local match "`s(`=regexs(0)')'"
				* noi di "Matched cond element: `match'"
				if regexm("`match'", "(mean|sum)\(([a-zA-Z0-9_ ]+)\)") { // is it a scale?
					local condScale "`=regexs(2)'"
					* noi di "Conditional scale: `condScale'"
					local inList: list miScale & condScale // check of the scale is in the list of scales
					if "`inList'" == "" {
						noi di in r "Scale `condScale' specified in option condimputed() is not present in the model"
						exit 489
					}
				}
				else { // is it an endogenous/covariate variable?
					if "`type'" == "cImp" {   // if endogenous
						local mylist "`miSadv'"
						local printType "condimputed()"
					}
					else {                    // if complete covariate
						local mylist "`covars'"
						local printType "condcomplete()"
					}
					
					capture confirm n `match'   // check whether match is numeric
					if _rc {                    // if match is not numeric
						if !`:list match in mylist' {  // if not in respective list
							noi di in r "Variable `match' specified in option `printType' is not present in the model"
							exit 489
						}
					}
				}		
			}
		}
	}
end


*** Parser of MODel
*** (sc1="logit, augment" sc2="pmm") and variations
capture program drop _parseMODel
program define _parseMODel, sclass

	args myinput type 

	local nlistex "[a-zA-Z]+[,]?[a-zA-Z0-9\(\)= ]*"
	local strregex "[a-zA-Z0-9\_\*]+[ ]*=[ ]*(\'|\")`nlistex'(\'|\")"

	while regexm(`"`myinput'"', `"`strregex'"') {
		local scale `=regexs(0)'
		local myinput = trim(subinstr(`"`myinput'"', `"`scale'"', "", .))
		gettoken sname model_opts: scale, parse("=")
		gettoken left model_opts: model_opts, parse("=")
		local model_opts = trim(`"`model_opts'"')
		local model_opts = subinstr(`"`model_opts'"', `"""',"",.)
		local model_opts = subinstr(`"`model_opts'"', `"'"',"",.)
		
		** Drop asterisk from sname
		local sname = subinstr("`sname'", "*", "", .)
		local sname = trim("`sname'")
		* noi di "`sname'"
		*** Post result
		sreturn local `sname'`type' `model_opts'
	}
end

*** Categorize the items of scales and print summary information
capture program drop _scaleItemCategorization
program define _scaleItemCategorization, sclass

	args scale isContScale catcutoff mincsize nacode timelevs
	 
	*** Specify temp names for scalars and matrices
	tempname vals freqs pCats nCats   
			
	*** Check item type as well as capture and report constant items and rare categories

	local bin  ""    // binary items
	local cat  ""    // multiple category items
	local cont ""    // continuous items
	
	local constant ""  // constant items
	local rare ""      // items with rare categories 
	local cuscont ""   // Items designated as continous by user

	local finalScale ""   // admitted items
	
	*** Collect all items of the scale
	unab myscale: `scale'*
	
	*** User assignment to continuous
	local userOverride: list scale in isContScale // is scale in user-defined?
	* noi di "Override: `userOverride'"
	if (`userOverride' == 1) {
		foreach item of local myscale {  // iterate over items of user-defined
			capture tab `item' if `item' ~= `nacode', matrow(`vals')
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
		*** Automatic assignment to all various types (may also have to look at the labels if they exist)
		foreach item of local myscale {  // iterate over items of scales
			
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
			capture tab `item' if `item' ~= `nacode', matrow(`vals') matcell(`freqs')
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

	*** Store the total number of time points
	local tlevs: word count `timelevs'
	
	*** Report results by scale
	noi di in y _n "********************************************************" _n ///
	"SUMMARY OF SCALE [`scale']" _n  ///
	"    Number of items: `:word count `myscale'' (`=`:word count `myscale''/`tlevs'' per time point)" _n ///
	"    Binary items: `:word count `bin''" _n ///
	"    Multiple category items: `:word count `cat''" _n ///
	"    Continuous items: " _n ///
	"        Auto detected: `:word count `cont''" _n ///
	"        User defined : `:word count `cuscont''" _n ///
	"   *Excluded items*: " _n ///
	"        Constant items: `constant'" _n ///
	"        Items with level count < `mincsize': `rare'" _n ///	
	"Final number of items: `:word count `finalScale''" _n ///
	"********************************************************"
	
	sreturn local finalScale "`finalScale'"
	sreturn local constant "`constant'"
	sreturn local rare "`rare'"
	sreturn local bin "`bin'"
	sreturn local mCat "`cat'"
	sreturn local contUI "`cuscont'" 
	
end

*** Creates the mean/sum score syntax for include()
capture program drop _meanSumInclude
program define _meanSumInclude, sclass

	args mylist scoretype timevar timelevs

	local include_items ""
	
	* noi di "`mylist'"
	* noi di "`timelevs'"
	
	foreach scale of local mylist {
		unab myitems: `scale'*
		* noi di "`myitems'"
		foreach tlev of local timelevs {
			local taggregs ""
			foreach item of local myitems {	
				if regexm("`item'", "^`scale'[a-zA-Z0-9_]*_`timevar'`tlev'$") {
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
	
	sreturn local include_items "`include_items'"
end


**** Rebuild conditions
capture program drop _buildCondition
program define _buildCondition, sclass
	
	args j timevar tp miDepVarsOriginal cov_invar cov_var
	
	if regexm("`j'", "(mean|sum)\(([a-zA-Z0-9_ ]+)\)") {
		local aggType "`=regexs(1)'"
		unab fullItemList: `=regexs(2)'*
		local timeItemList ""
		foreach item of local fullItemList { // collect items for same time period
			if regexm("`item'", ".+_`timevar'`tp'") {
				local timeItemList `=itrim("`timeItemList' `item'")'
			}
		}
		local sumItemList = subinstr("`timeItemList'", " ", "+", .)	
		if ("`aggType'" == "mean") {
			local len: word count `timeItemList'
			local expr "((`sumItemList')/`len')"
		}
		else if ("`aggType'" == "sum") {
			local expr "(`sumItemList')"
		}
	}
	else {  // this is failing to pick up y2 as a var!
		*noi di "`j'"
		*noi di "`cov_invar'"
		*noi di "`miDepVarsOriginal'"
		
		if ("`:list j in cov_invar'" == "1") { // check the stand-alone varlist and time invar covars
			local expr "`j'"
		}
		else if ("`:list j in miDepVarsOriginal'" == "1") | ("`:list j in cov_var'" == "1") {
			local expr "`j'_`timevar'`tp'"
		}
		else {
			capture confirm number `j'
			if _rc {
				* noi di _n "`j'"
				noi di in r "Value or variable `j' on the RHS of condition is invalid."
				error 489
			}
			else {
				local expr "`j'"
			}
		}
	}
	sreturn local expr "`expr'"
end


**** Slice and rebuild the conditional strings
capture program drop _imputationS2Cond
program define _imputationS2Cond, sclass

	args condimputed scale depvar timevar miDepVarsOriginal cov_invar cov_var depVarModOrig type
	
	_parseConditions `"`condimputed'"' "`type'"
	
	if regexm("`depvar'", "(.+)_`timevar'([0-9]+)$") {
		local stub "`=regexs(1)'"    // stub
		local tp "`=regexs(2)'"        // time period
	}
	if "`scale'" ~= "" {   // bypass to accommodate scale vars
		local stub "`scale'"
	}
	
	*** Obtain the list of all depVarModOrig
	if "`depVarModOrig'" ~= "" {
		unab asteriskList: `depVarModOrig'
		if `:list depvar in asteriskList' { // bypass to accomodate vars with wild cards
			local stub "`=subinstr("`depVarModOrig'", "*", "", .)'"
		}
	}
	
	if "cond_`stub'" ~= "" {
		local condLHS "`s(ifL`type'_`stub')'"   
		local condRHS "`s(ifR`type'_`stub')'"
		local wSigns "`s(ifS`type'_`stub')'"
		local bSigns "`s(ifB`type'_`stub')'"
	}

	*** Rebuilding the conditions
	local myCount = 1
	local condImp ""
	
	*** Rebuilding LHS
	foreach j of local condLHS {
		*** Construct the LHS
		noi _buildCondition "`j'" "`timevar'" "`tp'" "`miDepVarsOriginal'" "`cov_invar'" "`cov_var'"
		local lhsExpr `s(expr)'
		
		*** Rebuilding RHS
		local right: word `myCount' of `condRHS'
		noi _buildCondition "`right'" "`timevar'" "`tp'" "`miDepVarsOriginal'" "`cov_invar'" "`cov_var'"
		local rhsExpr `s(expr)'

		*** Building the dependent variable-specific condition
		local condImp `"`condImp' `lhsExpr' `:word `myCount' of `wSigns'' `rhsExpr' `:word `myCount' of `bSigns''"'
		local ++myCount
	}
	if "`condImp'" ~= "" {
		if "`type'" == ""  {
			sreturn local condImp "cond(if`condImp')"
		}
		else {
			sreturn local cond`type' "if`condImp'"
		}
	}
	else {
		sreturn local condImp ""
		sreturn local cond`type' ""
	}
	
end

*** Create a list of variables for all periods
capture program drop _createAllPeriods
program define _createAllPeriods, sclass
	
	args inList timevar
	
	local outList ""
	if "`inList'" ~= "" {
		foreach var of local inList {
			local wCard = strpos("`var'", "*")
			if `wCard' ~= 0 {  // if there is a wildcard
				local preFix = substr("`var'", 1, `=`wCard' -1') // get the prefix
				fvunab placeholder: `preFix'*	
				local outList "`outList' `placeholder'"
			}
			else {
				*** Include only variables that have the `var' part or `var'_timevar part
				fvunab placeholder: `var'*	
				foreach myVar of local placeholder {
					if regexm("`myVar'", "`var'(_`timevar'[0-9]+)?$") {
						local outList "`outList' `myVar'"
					}
				}
			}
		}	
	}
	
	sreturn local expandedList "`outList'"
	
end


*** Check variable name length
capture program drop _checkVarNameLength
program define _checkVarNameLength
	
	args allVars timevar allowedLen
	qui {
		local timeVarLen = length("`timevar'")
			
		sum `timevar'
		local timeLen =length("`r(max)'")
		
		foreach var of local allVars {
			local varNameLen = length("`var'")
			local newVarLen = `varNameLen' + `timeVarLen' + `timeLen' + 1
			if (`newVarLen' > `allowedLen') {
				noi di _n in r "Please, shorten the name of variable `var' to "
				noi di _c "less than `=`allowedLen' - (`timeVarLen' + `timeLen' + 1)' characters."
				noi di _n "Alternatively, you may wish to shorten the name of '`timevar'' and try again."
				exit 489
			}
		}
	}
end


*** Display function for variables included in the model
capture program drop _displayAllVars
program define _displayAllVars

	args scales sadv covInvar covVar
	
	if "`scales'" ~= "" {
		noi di in y _n "********************************************************" _n ///
		"Scales in the imputation model:" _n ///
		"      `scales'" _n ///
		"<more information follows below> "
		noi di "********************************************************"
	}
	else {
		noi di _n in y "********************************************************" _n ///
		"No scales in the imputation model "
		noi di "********************************************************"	
	}
	
	if "`sadv'" ~= "" {
		noi di in y _n "********************************************************" _n ///
		"Stand-alone dependent variables in the imputation model:" _n ///
		"      `sadv'" 
		noi di "********************************************************"
	}
	else {
		noi di _n in y "********************************************************" _n ///
		"No stand-alone variables in the imputation model "
		noi di "********************************************************"	
	}
	
	if ("`covInvar'" ~= "" | "`covVar'" ~= "") {
		noi di _n in y "********************************************************" _n ///
		"Covariates in the imputation model: " _n ///
		"    Time-invariant: `covInvar'" _n ///
		"    Time-variant  : `covVar'"
		noi di "********************************************************"	
	}
	else {
		noi di _n in y "********************************************************" _n ///
		"No covariates in the imputation model "
		noi di "********************************************************"	
	}
end

*** Expand variable names with wild cards
capture program drop _wildCardExpand
program define _wildCardExpand, sclass

	args var allCovariates
	
	*** Check if there is a wildcard in rVar
	local wCard = strpos("`var'", "*")
	if `wCard' ~= 0 {  // if there is a wildcard
		local preFix = substr("`var'", 1, `=`wCard' -1') // get the prefix
		unab varList: `preFix'*
		foreach sadv of local varList {
			if !`:list sadv in allCovariates' {
				local sadvList "`sadvList' `sadv'"
			}
		}
	}
	else {  // there is no wildcard
		local sadvList "`sadvList' `var'"
	}
	sreturn local sadvList "`sadvList'"
	
end


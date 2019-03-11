
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

	args myinput type
	
	local nlistex "[a-zA-Z(-0-9)_\(\)<>~=\.\&\| ]+"
	local strregex "[a-zA-Z0-9\_]+[ ]*=[ ]*(\'|\")`nlistex'(\'|\")"
	local myvars "([a-zA-Z0-9_\(\) ]+)([<>~=]+)([a-zA-Z(-0-9)_\(\) ]+)(\|?\&?)" // [a-zA-Z0-9_]\|\&\.\(\)]+"
	
	*noi di `"`myinput'"'
	*noi di "TEST 0"
	
	while regexm(`"`myinput'"', `"`strregex'"') {
		local scale `=regexs(0)'
		*noi di "TEST 1"
		*noi di `"`scale'"'
		local myinput = trim(subinstr(`"`myinput'"', `"`scale'"', "", .))
		gettoken sname cond: scale, parse("=")
		gettoken left cond: cond, parse("=")
		local cond = trim(`"`cond'"')
		local cond = subinstr(`"`cond'"', `"""',"",.)
		local cond = subinstr(`"`cond'"', `"'"',"",.)
		local sname = trim("`sname'")
		
		*noi di "`sname'"
		
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

	args condition type miScale miSadv covars
	
	_parseConditions `"`condition'"'
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
	local strregex "[a-zA-Z0-9\_]+[ ]*=[ ]*(\'|\")`nlistex'(\'|\")"

	while regexm(`"`myinput'"', `"`strregex'"') {
		local scale `=regexs(0)'
		local myinput = trim(subinstr(`"`myinput'"', `"`scale'"', "", .))
		gettoken sname model_opts: scale, parse("=")
		gettoken left model_opts: model_opts, parse("=")
		local model_opts = trim(`"`model_opts'"')
		local model_opts = subinstr(`"`model_opts'"', `"""',"",.)
		local model_opts = subinstr(`"`model_opts'"', `"'"',"",.)
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


*** Slice conditions
capture program drop _parseCondition
program define _parseCondition, sclass

	args myinput type
	
	local nlistex "[a-zA-Z(-0-9)_\(\)<>~=\.\&\| ]+"
	local strregex "[a-zA-Z0-9\_]+[ ]*=[ ]*(\'|\")`nlistex'(\'|\")"
	local myvars "([a-zA-Z0-9_\(\) ]+)([<>~=]+)([a-zA-Z(-0-9)_\(\) ]+)(\|?\&?)" // [a-zA-Z0-9_]\|\&\.\(\)]+"
	
	*noi di `"`myinput'"'
	*noi di "TEST 0"
	
	while regexm(`"`myinput'"', `"`strregex'"') {
		local scale `=regexs(0)'
		*noi di "TEST 1"
		*noi di `"`scale'"'
		local myinput = trim(subinstr(`"`myinput'"', `"`scale'"', "", .))
		gettoken sname cond: scale, parse("=")
		gettoken left cond: cond, parse("=")
		local cond = trim(`"`cond'"')
		local cond = subinstr(`"`cond'"', `"""',"",.)
		local cond = subinstr(`"`cond'"', `"'"',"",.)
		local sname = trim("`sname'")
		
		*noi di "`sname'"
		
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

	args condimputed scale depvar timevar miDepVarsOriginal cov_invar cov_var type
	
	noi _parseCondition `"`condimputed'"' "`type'"
	*noi sreturn list
	if regexm("`depvar'", "(.+)_`timevar'([0-9]+)$") {
		local depvar "`=regexs(1)'"
		local tp "`=regexs(2)'"
	}
	if "`scale'" ~= "" {   //bypass to accommodate scale vars
		local depvar "`scale'"
	}

	if "cond_`depvar'" ~= "" {
		local condLHS "`s(ifL`type'_`depvar')'"   
		local condRHS "`s(ifR`type'_`depvar')'"
		local wSigns "`s(ifS`type'_`depvar')'"
		local bSigns "`s(ifB`type'_`depvar')'"
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
			fvunab placeholder: `var'*	
			local outList "`outList' `placeholder'"
			
			/*
			*** Include only variables that have the `var' part or `var'_timevar part
			foreach myVar of local placeholder {
				if regexm("`myVar'", "`var'(_`timevar'[0-9]+)?$") {
					noi di "`myVar'"
					local outList "`outList' `myVar'"
				}
			}
			*/
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
	
	

exit

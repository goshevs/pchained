
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
	
	noi di "`model'"
	
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
		noi di "`includeVars'"
		
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
				noi di "Matched cond element: `match'"
				if regexm("`match'", "(mean|sum)\(([a-zA-Z0-9_ ]+)\)") { // is it a scale?
					local condScale "`=regexs(2)'"
					noi di "Conditional scale: `condScale'"
					local inList: list miScale & condScale // check of the scale is in the list of scales
					if "`inList'" == "" {
						noi di in r "Scale `condScale' specified in option condimputed() is not present in the model"
						exit 489
					}
				}
				else { // is it an endogenous/covariate variable?
					if "`type'" == "cImp" {
						local mylist "`miSadv'"
						local printType "condimputed()"
					}
					else {
						local mylist "`covars'"
						noi di "My covariates: `covars'" 
						local printType "condcomplete()"
					}
					capture confirm v `match'
					if _rc {
						capture confirm n `match'  // conditoins have to be numeric!!!
						if _rc {
							noi di in r "Variable `match' specified in option `printType' is not present in the model"
							exit 489
						}	
					}
					else {	
						if !`:list match in mylist' {
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


capture program drop _scaleItemCategorization
program define _scaleItemCategorization

	args scale isContScale
	
	*** Specify temp names for scalars and matrices
	tempname vals freqs pCats nCats   
			
	*** Check item type as well as capture and report constant items and rare categories

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
	local userOverride: list scale in isContScale // is scale in user-defined?
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
		*** Automatic assignment to all various types (may also have to look at the labels if they exist)
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
	
	sreturn local finalScale "`finalScale'"
	sreturn local constant "`constant'"
	sreturn local rare "`rare'"
	sreturn local bin "`bin'"
	sreturn local cuscont "`cuscont'" 
	
end










exit



capture program drop _parseConditions
program define _parseConditions, sclass

	args myinput type


end

	*** Disaggregate input for conditional imputation
		_parseConditions `"`condimputed'"'
		* noi sreturn list
		
		*** Collect all left and right sides
		local lSide ""
		local rSide ""
		foreach el in `:s(macros)' {
			if regexm("`el'", "^ifL_.+") { // left side
				
			}
			else if regexm("`el'", "^ifR_.+") { // right side
				local match "`s(`=regexs(0)')'"
				if regexm("`match'", "(mean|sum)\(([a-zA-Z0-9_ ]+)\)") {
					local condScale "`=regexs(2)'"
					local inNameList: list miScale & condScale
					if "`inNameList'" == "" {
						noi di in r "Scale `condScale' specified in option condimputed() is not present in the model"
						exit 489
					}
				}
				else {
					local rSide "`rSide' `match'"
				}
			}
		}
		
		* noi di "`lSide'" _n "`rSide'"

		*** Separates variables in the dataset from other stuff
		local vars "`lSide' `rSide'"
		local condVars ""
		foreach i of local vars {
			capture unab test: `i'
			if !_rc { // no error
				*** Add to variable list
				local condVars "`condVars' `i'"
			}
		}
				
		local condVars: list uniq condVars  // remove repeats
		
		noi di "Conditioning variables: `condVars'"
		exit
		
		
		
		*** Check whether cond vars are already present in the list of other vars
		local checkCondVar: list condVars & miSadv
		local checkCondVar: list condVars - checkCondVar
		
		* noi di "`checkCondVar'"
		* noi di "`condVars'"
		* noi di "`miDepVarsOriginal'"
		
		if "`checkCondVar'" != "" {
			noi di in r "Variable(s) `checkCondVar' in conditional imputation is(are) not included in the model"
			exit 489
		}
		


*** Parser of the user input with multiple arguments of the type
*** (sc1="logit, augment" sc2="pmm") and variations

capture program drop _parse_model
program define _parse_model, sclass

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










exit






*** Compare elements of lists and print elements that differ
capture program drop compare_lists
program define compare_lists, sclass
	args list1 list2
	
	local isect: list list1 & list2
	local union: list list1 | list2
	local lDiff: list union - isect // LONGER SHOULD BE FIRST!
	* di "`lDiff'"
	sreturn local differences `lDiff'

end



capture program drop _parse_ovar_model
program define _parse_ovar_model, sclass
	
	args model
	
	noi di "`model'"
	
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
		noi di "`includeVars'"
		
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

/*
*** Parses input anything 
capture program drop _input_parser
program define _input_parser, sclass

	args input_string
	
	sreturn clear
	
	local regex_scale "^[a-zA-Z0-9_ ]+"
	local model_exp "([a-zA-Z0-9_]+[a-zA-Z0-9_. ]*[ ]?)"
	local opts_exp "(,[ ]*([a-zA-Z]+([ ]|\(([a-zA-Z0-9_. ]+|mean\([a-zA-Z0-9_ ]+\)|sum\([a-zA-Z0-9_ ]+\))+\)[ ]?|))+)?"
		
	local regex_model "`model_exp'`opts_exp'"
	
	if regexm(`"`input_string'"', `"`regex_scale'"') {
		local namelist `= regexs(0)'
		local input_string = trim(subinstr(`"`input_string'"', `"`namelist'"', "", 1))
		
		sreturn local namelist "`namelist'"
	}
	
	local count = 1
	while regexm(`"`input_string'"', `"`regex_model'"') {
		local expression `= regexs(0)'
		local input_string = trim(subinstr(`"`input_string'"', `"(`expression')"', "", .))
		sreturn local ovar`count' `=regexs(0)'
		local ++count
		
	}
end


*** Parser of sadv_models
capture program drop _parse_ovar_model
program define _parse_ovar_model, sclass
	
	args model
		
	gettoken eq opts: model, parse(",")
	local ovar `:word 1 of `eq''
	local ovar = trim("`ovar'")
	
	gettoken left covs: eq
	local covs = trim("`covs'")
	
	gettoken right opts: opts, parse(",")
	local opts = trim("`opts'")
	
	if regexm("`opts'", "omit\(([a-zA-Z0-9_. ]+)\)") {
		local omitOpt `=regexs(0)'
		local omitVars `=regexs(1)'
	}
	
	if regexm("`opts'", "include\((([a-zA-Z0-9_. ]+|mean\([a-zA-Z0-9_ ]+\)|sum\([a-zA-Z0-9_ ]+\))+)\)") {
		local includeOpt `=regexs(0)'
		local includeVars `=regexs(1)'
		noi di "`includeVars'"
		
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
*/

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
capture program drop _parse_condition
program define _parse_condition, sclass

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
capture program drop _construct_conditions
program define _construct_conditions, sclass
	
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
capture program drop _condimputation
program define _condimputation, sclass

	args condimputed scale depvar timevar miDepVarsOriginal cov_invar cov_var type
	
	noi _parse_condition `"`condimputed'"' "`type'"
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
		noi _construct_conditions "`j'" "`timevar'" "`tp'" "`miDepVarsOriginal'" "`cov_invar'" "`cov_var'"
		local lhsExpr `s(expr)'
		
		*** Rebuilding RHS
		local right: word `myCount' of `condRHS'
		noi _construct_conditions "`right'" "`timevar'" "`tp'" "`miDepVarsOriginal'" "`cov_invar'" "`cov_var'"
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

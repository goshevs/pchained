
/*
sreturn clear

*** Rules are:
*** no cross-temporal conditioning

local test "s1_i = 'if mean(s2_i) < sum(s2_i) | y2 > 3' s2_i = 'if mean(s1_i) > 2' y2 = 'if y3 < 20' y3 = 'if y2 ~= mean(s1_i) | y2 < sum(s2_i)'"
local namelist "bea_ ss_ pe_"

local test1 "s1_i = 'if x1 < 10 | inlist(x2, 0, 1, 3, 4, 5, 7)'"

*/
*** Parser of the user input with multiple arguments of the type
*** (sc1="logit, augment" sc2="pmm") and variations

capture program drop _parse_condition
program define _parse_condition, sclass

	args myinput type

	local nlistex "[a-zA-Z0-9_\(\)<>~=.&| ]+"
	local strregex "[a-zA-Z0-9\_]+[ ]*=[ ]*(\'|\")`nlistex'(\'|\")"
	local myvars "([a-zA-Z0-9_\(\) ]+)([<>~=]+)([a-zA-Z0-9_\(\) ]+)(\|?\&?)" // [a-zA-Z0-9_]\|\&\.\(\)]+"
	
	while regexm(`"`myinput'"', `"`strregex'"') {
		local scale `=regexs(0)'
		local myinput = trim(subinstr(`"`myinput'"', `"`scale'"', "", .))
		gettoken sname cond: scale, parse("=")
		gettoken left cond: cond, parse("=")
		local cond = trim(`"`cond'"')
		local cond = subinstr(`"`cond'"', `"""',"",.)
		local cond = subinstr(`"`cond'"', `"'"',"",.)
		local sname = trim("`sname'")
		
		* noi di "`sname'"
		
		*** parsing the if condition
		local ifL "" // left side
		local ifS "" // signs
		local ifR "" // right side
		local ifB "" // between
		* noi di "`cond'"
		
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

/*
_parse_condition "`test'"
sreturn list

*/

**** Reconstruct the conditions
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
		* noi di "`j'"
		* noi di "`cov_invar'"
		* noi di "`miDepVarsOriginal'"
		
		if ("`:list j in cov_invar'" == "1") { // check the stand-alone varlist and time invar covars
			local expr "`j'"
		}
		else if ("`:list j in miDepVarsOriginal'" == "1") | ("`:list j in cov_var'" == "1") {
			local expr "`j'_`timevar'`tp'"
		}
		else {
			capture confirm number `j'
			if _rc {
				noi di _n "`j'"
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


**** Slice and rebuild the conditional imputation string
capture program drop _condimputation
program define _condimputation, sclass

	args condimputed scale depvar timevar miDepVarsOriginal cov_invar cov_var type
	
	_parse_condition `"`condimputed'"' "`type'"
		
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


exit


/*
local test "`s(ifS_y3)'"
foreach i of local test {
	noi di in y "`i'"
}
*/


*** Identifying whether variable or not
local test1 "`s(ifR_y3)'"
foreach i of local test1 {
	noi di in y "`i'"
	capture unab test: `i'
	if _rc {
		noi di "not a variable"
	}
	
}

exit




capture program drop _parse_test
program define _parse_test, sclass
	args myinput
	
	


end




_parse_condition "`test'"
sreturn list



capture program drop _parse_model_condition
program define _parse_model_condition, sclass

	args myinput 

	local nlistex "[a-zA-Z0-9_\(\)<>~=.&| ]+"
	local strregex "[a-zA-Z0-9\_]+[ ]*=[ ]*(\'|\")`nlistex'(\'|\")"

	while regexm(`"`myinput'"', `"`strregex'"') {
		local scale `=regexs(0)'
		local myinput = trim(subinstr(`"`myinput'"', `"`scale'"', "", .))
		gettoken sname cond: scale, parse("=")
		gettoken left cond: cond, parse("=")
		local cond = trim(`"`cond'"')
		local cond = subinstr(`"`cond'"', `"""',"",.)
		local cond = subinstr(`"`cond'"', `"'"',"",.)
		local sname = trim("`sname'")
		* noi di "`sname'"
		*** Post result
		sreturn local cond_`sname' `cond'

	}
end






_parse_condition "`test'"
foreach scale of local namelist {
	local condStatement `"`s(cond_`scale')'"'
	if `"`condStatement'"' ~= "" {
		_parse_model_condition `"`condStatement'"'
		
		
		local extraModels = 1
	local miDepVarsOriginal ""
	local miCovVars ""
	while `"`s(ovar`extraModels')'"' ~= "" {
		_parse_ovar_model "`s(ovar`extraModels')'"
		* noi sreturn list
		local miDepVarsOriginal "`miDepVarsOriginal' `s(depv)'"
		local miCovVars "`miCovVars' `s(covs)'"
		local ++extraModels
	}
	
	foreach item of local myscale { 
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
	}			
}
exit
				
				
				




capture program drop _parse_condition
program define _parse_condition, sclass

	args myinput 

	local nlistex "[a-zA-Z0-9_\(\)<>~=.&| ]+"
	local strregex "[a-zA-Z0-9\_]+[ ]*=[ ]*(\'|\")`nlistex'(\'|\")"

	while regexm(`"`myinput'"', `"`strregex'"') {
		local scale `=regexs(0)'
		local myinput = trim(subinstr(`"`myinput'"', `"`scale'"', "", .))
		gettoken sname cond: scale, parse("=")
		gettoken left cond: cond, parse("=")
		local cond = trim(`"`cond'"')
		local cond = subinstr(`"`cond'"', `"""',"",.)
		local cond = subinstr(`"`cond'"', `"'"',"",.)
		local sname = trim("`sname'")
		* noi di "`sname'"
		*** Post result
		sreturn local cond_`sname' `cond'

	}
end





exit


CONDImputation(ss_ = "if smoker == 1" y2 = "if mean(ss_) < 1.4" y3 = "if (sum(bea_) < .5 & mean(ss_) > 5) | smoker ~= 1")
cond(associative_array)


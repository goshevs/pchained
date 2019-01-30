
sreturn clear

*** Rules are:
*** no cross-temporal conditioning

local test "ss_ = 'if smoker == 1' y2 = 'if mean(ss_) < race' y3 = 'if (sum(bea_) < 1.5 & mean(pe_) > 5) | smoker ~= 1'"
local namelist "bea_ ss_ pe_"

*** Parser of the user input with multiple arguments of the type
*** (sc1="logit, augment" sc2="pmm") and variations

capture program drop _parse_condition
program define _parse_condition, sclass

	args myinput 

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
		noi di "`cond'"
		
		local mycond = regexr(`"`cond'"', "^[ ]*if[ ]+", "")
		while regexm(`"`mycond'"', `"`myvars'"') {
			local ifL "`ifL' `=regexs(1)'"
			local ifR "`ifR' `=regexs(3)'"
			local ifS "`ifS' `=regexs(2)'"
			local ifB "`ifB' `=regexs(4)'"
			local mycond = subinstr(`"`mycond'"', "`=regexs(0)'", "",.)
		}	
		*** Post result
		sreturn local cond_`sname' `cond'
		sreturn local ifL_`sname' `=itrim("`ifL'")'
		sreturn local ifR_`sname' `=itrim("`ifR'")'
		sreturn local ifS_`sname' `"`=trim("`ifS'")'"'
		sreturn local ifB_`sname' `"`=trim("`ifB'")'"'
	}
end

_parse_condition "`test'"
sreturn list

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


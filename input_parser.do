
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
	
	if regexm("`opts'", "((mean\([a-zA-Z0-9_ ]+\)|sum\([a-zA-Z0-9_ ]+\))[ ]*)+") {
		local includeVars `=regexs(0)'
	}
		
	if regexm("`opts'", "omit\(([a-zA-Z0-9_. ]+)\)") {
		local omitOpt `=regexs(0)'
		local omitVars `=regexs(1)'
	}
	
	if regexm("`opts'", "include\(([a-zA-Z0-9_. ]+|mean\([a-zA-Z0-9_ ]+\)|sum\([a-zA-Z0-9_ ]+\))+\)") {
		local includeOpt `=regexs(0)'
	}
	
	local remaningOpts = trim(subinstr("`opts'", "`includeOpt'","", .))
	local remaningOpts = trim(subinstr("`remaningOpts'", "`omitOpt'","", .))
		
	noi di "`omitOpt'"
	noi di "`includeOpt'"
	
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


local test "s1_ s2_ (y2, include(mean(s1_i)) omit(x1 i.x2 x3 y1)) (y3 i.yx x1 i.yz, include(mean(s1_i)) omit(x3 y1) noimputed) (d3 hu3 nd1) (dem2 i.ys2, include(g1 mean(s1_ s3_) sum(s2_)) omit(s1_ s2_) noimputed) (h2 edu2 inc34, noimputed include(s3_) noimputed omit(s1_ s2_)) (gen4)"
_input_parser "`test'"
sreturn list
_parse_ovar_model "`s(ovar1)'"
sreturn list


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
				if regexm("`item'", "^`scale'[a-z0-9]*_`timevar'`tlev'$") {
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

_meanSumInclude "s1_i" "mean" "1 2 3"
sreturn list






capture program drop test
program define test

	args number
	
	c_local number = `number'
end

test 2
macro list
di "`number'"



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
		local covarsWide ""
		if "`commoncov'" ~= "" {
			*** Build list of covariates in wide format
			foreach cov of local commoncov {
				fvunab mycov: `cov'*	
				foreach cvar of local mycov {
					if !`:list cvar in covarsWide' {
						local covarsWide "`covarsWide' `mycov'"
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
		
		
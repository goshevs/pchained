* Simulating data for Plumpton with -mi impute chained-
* Author: Simo Goshev
* Date: 7/11/2018
* Version: 0.1
*
*
*



********************************************************************************
*** Data
********************************************************************************

capture program drop simdata
program define simdata

	clear
	set more off
	set seed 123456

	********************************************************************************
	*** Generate dummy data
	********************************************************************************

	set obs 200
	gen id = _n
	expand 3
	bys id: gen time = _n

	bys id: gen x1 = round(5 + 5 * runiform()) if _n == 1
	bys id: replace x1 = x1[_n -1] + 1 if _n > 1

	bys id: gen ih = rnormal() if _n ==1
	bys id: replace ih = ih[1] if _n > 1

	** Time-invariant categorical var
	bys id: gen x2 = round(1 + 4*runiform() + ih) if _n ==1
	bys id: replace x2 = cond(x2[1] < 0, 0, x2[1])
	bys id: replace x2 = x2[1] if _n > 1

	** Time-variant categorical var
	bys id: gen x3 = round(1 + 3*runiform() + ih) if _n ==1
	bys id: replace x3 = cond(x3[1] < 0, 0, x3[1])
	bys id: replace x3 = cond(runiform() > 0.3, x3[_n -1] + 1, x3[_n-1]) if _n > 1 


	bys id: gen x4 = round(1 + 3*runiform() + ih) if _n ==1
	bys id: replace x4 = cond(x4[1] < 0, 0, x4[1])
	bys id: replace x4 = cond(runiform() > 0.3, x4[_n -1] + 1, x4[_n-1]) if _n > 1 



	** Scale 1:
	forval i = 1/3 {
		bys id: gen s1_i`i' = floor(2 * runiform() + ih) 
		replace s1_i`i' = 0 if s1_i`i' < 0
	}

	** Scale 2:
	forval i = 1/2 {
		bys id: gen s2_i`i' = floor(7 * runiform() + ih) 
		replace s2_i`i' = 0 if s2_i`i' < 0
	}


	** Scale 3:
	forval i = 1/4 {
		bys id: gen s3_i`i' = floor(2 * runiform() + ih) 
		replace s3_i`i' = 0 if s3_i`i' < 0
	}


	gen y = 0.5 + 1.5 * x1 - 0.5 * x2 + 1.5 * x3 + ih + rnormal()

	drop ih

	sum


	********************************************************************************
	*** Introduce missingness
	********************************************************************************

	foreach var of varlist s* {
		replace `var' = . if runiform() <0.1
	}

	misstable sum s*

end
***

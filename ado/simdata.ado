* Simulating data for -pchained-
* Developers: Simo Goshev, Zitong Liu
* Version: 0.5
*
*
*
*
*

********************************************************************************
*** Data
********************************************************************************

capture program drop simdata
program define simdata
	args samSize waves missWave
	
	qui {
		clear
		set more off
		set seed 123456

		********************************************************************************
		*** Generate dummy data
		********************************************************************************

		set obs `samSize'
		gen id = _n
		expand `waves'
		bys id: gen time = _n

		** Generate weight
		bys id: gen weight = runiform() if _n ==1
		bys id: replace weight = weight[1]
		
		** Generate covariates
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
		bys id: replace x3 = cond(runiform() > 0.5, x3[_n -1] + 1, x3[_n-1]) if _n > 1 


		bys id: gen x4 = round(1 + 3*runiform() + ih) if _n ==1
		bys id: replace x4 = cond(x4[1] < 0, 0, x4[1])
		bys id: replace x4 = cond(runiform() > 0.3, x4[_n -1] + 1, x4[_n-1]) if _n > 1 
		bys id: gen x5 = rnormal()
		bys id: gen x6 = rnormal()
		
		** Generate ovars and covars
		bys id: gen y2 = abs(rnormal())
		bys id: gen y3 = round(rnormal())
		bys id: gen y4 = round(rnormal())
		bys id: gen y5 = cond(y4 >= 0, cond(runiform() > 0.2, rnormal(), .), .)
		bys id: gen y6 = cond(x5 >= 0, cond(runiform() > 0.2, rnormal(), .), .)
		gen y7 = round(1 + 3 * runiform())
		recast int y7		
		
		bys id: gen ys1 = cond(x6 >= 0, cond(runiform() > 0.2, rnormal(), .), .)
		bys id: gen ys2 = cond(x6 >= 0, cond(runiform() > 0.2, rnormal(), .), .)
		
		bys id: gen yx = round(runiform())
		bys id: gen yz = round(0 + 3 * runiform())
				
		** Scale 1:
		forval i = 1/3 {
			bys id: gen s1_i`i' = floor(2 * runiform() + ih) 
			replace s1_i`i' = 0 if s1_i`i' < 0
		}

		** Scale 2:
		forval i = 1/2 {
			bys id: gen s2_i`i' = floor(5 * runiform() + ih) 
			replace s2_i`i' = 0 if s2_i`i' < 0
		}


		** Scale 3:
		forval i = 1/4 {
			bys id: gen s3_i`i' = round(runiform()) 
		}

		** Scale 4:
		forval i = 1/4 {
			bys id: gen s4_i`i' = (50 * runiform() + ih) 
			replace s4_i`i' = 0 if s4_i`i' < 0
		}
		
		gen y1 = 0.5 + 1.5 * x1 - 0.5 * x2 + 1.5 * x3 + ih + rnormal()
		
		
		** Scale 5 (conditional on x5):
		gen mydraw = runiform()
		forval i = 1/5 {
			bys id: gen s5_i`i' = cond(x5[1] >= 0, cond(mydraw[1] > 0.3, round(5 * runiform() + ih), .), .) 
			replace s5_i`i' = 0 if s5_i`i' < 0
		}

		** Scale 6 (conditional on mean(s1_i)):
		egen mymean = rowmean(s1_i*)
		replace mydraw = runiform()
		
		forval i = 1/3 {
			bys id: gen s6_i`i' = cond(mymean >= 1, cond(mydraw > 0.3, round(5 * runiform() + ih), .), .) 
			replace s6_i`i' = 0 if s6_i`i' < 0
		}
		
		drop ih mydraw mymean

		
		********************************************************************************
		*** Introduce missingness
		********************************************************************************

		foreach var of varlist s* y2 y3 y4 {
			replace `var' = . if runiform() <0.05
		}

		* misstable sum s*
		replace y5 = . if y4 == .
		
		egen mymiss = rowmiss(s1_i*)
		egen mymean = rowmean(s1_i*)
		replace mymean = . if mymiss > 0
		
		foreach var of varlist s6_i* {
			replace `var' = . if mymean ==.
		}
		
		drop mymiss mymean
		
		replace y7 = . if runiform() < 0.10
		
		if "`missWave'" ~= "" {
			replace y7 = . if time == `missWave'		
			replace s2_i1 = . if time == `missWave'
			replace s2_i2 = . if time == `missWave'
		}
		
		********************************************************************************
		*** Split the sample into two groups
		********************************************************************************
		
		bys id: gen group = round(runiform()) if _n == 1
		bys id: replace group = group[1]
		
		********************************************************************************
		*** Label conditional variables
		********************************************************************************
		
		label var y5 "recorded if y4 >=0"
		label var y6 "recorded if x5 >=0"
		label var ys1 "recorded if x6 >=0"
		label var ys2 "recorded if x6 >=0"
		
		foreach var of varlist s5_* {
			label var `var' "recorded if x5_time1 >= 0"
		}
		foreach var of varlist s6_* {
			label var `var' "recorded if mean(s1_i) > 0"
		}
			
	}
end
***

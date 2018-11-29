* Simulating data for pchained --> Plumpton with -mi impute chained-
* Developers: Simo Goshev, Zitong Liu
* Version: 0.23
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
	args samSize waves
	
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
		bys id: replace x3 = cond(runiform() > 0.3, x3[_n -1] + 1, x3[_n-1]) if _n > 1 


		bys id: gen x4 = round(1 + 3*runiform() + ih) if _n ==1
		bys id: replace x4 = cond(x4[1] < 0, 0, x4[1])
		bys id: replace x4 = cond(runiform() > 0.3, x4[_n -1] + 1, x4[_n-1]) if _n > 1 

		
		** Generate ovars and covars
		bys id: gen y2 = round(1 + 6*runiform() + ih) if _n ==1
		bys id: replace y2 = cond(y2[1] < 0, 0, y2[1])
		bys id: replace y2 = y2[1] if _n > 1

		bys id: gen y3 = round(5 + 5 * runiform()) if _n == 1
		bys id: replace y3 = y3[_n -1] + 1 if _n > 1
		
		bys id: gen yx = round(5 + 5 * runiform()) if _n == 1
		bys id: replace yx = yx[_n -1] + 1 if _n > 1
		
		bys id: gen yz = round(5 + 5 * runiform()) if _n == 1
		bys id: replace yz = yz[_n -1] + 1 if _n > 1
		
				
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

		drop ih

		sum


		********************************************************************************
		*** Introduce missingness
		********************************************************************************

		foreach var of varlist s* y2 y3 {
			replace `var' = . if runiform() <0.05
		}

		* misstable sum s*
		
		
		********************************************************************************
		*** Split the sample in two groups
		********************************************************************************
		
		bys id: gen group = round(runiform()) if _n == 1
		bys id: replace group = group[1]
	}
end
***

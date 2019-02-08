
simdata 500 3
pchained s1_i s2_i (y2 x2, noimputed) (y3 i.yx x1 i.yz, include(y2 mean(s1_i))), ///
	          i(id) t(time) scalecov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456)) ///
			  mod(s1_i = "pmm, knn(3)" y2 = "regress" y3 = "regress") ///
			  condi(s1_i = "if mean(s2_i) < sum(s2_i) | y2 > 3" y2 = "if y3 < 20" y3 = "if y2 ~= mean(s1_i) | y2 < sum(s2_i)") ///
			  condc(s1_i = "if x1 < 10" y3 = "if x2 < 1") print suspend
			  

			  
			  /*
Problem is with the stand-alone variable model wirting out part		  
regress , 

regress  , 

regress   , 
if x2 < 1 
regress if x2 < 1 , 
if x2 < 1 
regress if x2 < 1  if x2 < 1 , 
if x2 < 1 
regress if x2 < 1  if x2 < 1  if x2 < 1 , 
*/

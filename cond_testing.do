
simdata 500 3
pchained s1_i s2_i (y2 x2, noimputed) (y4 i.yx x1 i.yz x5, include(y2 mean(s1_i))) (y5 i.yx x1 i.yz x5, include(y2 y4)) (y6 x2, include(y5)), ///
	          i(id) t(time) scalecov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456)) ///
			  mod(s1_i = "pmm, knn(3)" y2 = "regress" y4 = "regress" y5 = "pmm, knn(3)" y6 = "regress") ///
			  condi(y5 = "if y4 > -1") ///
			  condc(y6 = "if x5 > 1")
			  
			  
			  
			  
			  
			  
			  
/*			  
pchained s1_i s2_i (y2 x2, noimputed) (y3 i.yx x1 i.yz, include(y2 mean(s1_i))), ///
	          i(id) t(time) scalecov(x1 i.x2 x3 y1) mio(add(1) chaindots rseed(123456)) ///
			  mod(s1_i = "pmm, knn(3)" y2 = "regress" y3 = "regress") ///
			  condi(s1_i = "if mean(s2_i) < sum(s2_i) | y2 > 3" y2 = "if y3 < 20" y3 = "if y2 ~= mean(s1_i) | y2 < sum(s2_i)") ///
			  condc(s1_i = "if x1 < 10" y3 = "if x2 < 1") print suspend
*/

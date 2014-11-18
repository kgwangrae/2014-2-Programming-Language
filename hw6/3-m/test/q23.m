let rec f = fn x =>          
        if x = 0 then 0      
					        else if x = 1 then 1 
										        else (f(x-1) + f(x-2)) 
															in                            
															    write f(10)              
																end   

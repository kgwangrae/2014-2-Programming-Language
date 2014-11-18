let 
rec cp = fn x => 
let 
rec bb = fn y=> 
if y =0 then 0 
else (y+ bb(y-1)) 
	in 
	if (x = 1540) then 1 
	else ((write x) 
		;cp (bb x)) 
	end 
	in 
	write cp(4) 
end 

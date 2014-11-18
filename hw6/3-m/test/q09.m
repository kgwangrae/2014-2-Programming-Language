let 
 rec lsum = fn x => 
   if (x = 0) then 
     0
   else
     (x + lsum (x - 1))
 rec godiag = fn x => 
   if (x = 0) then 
     0
   else
     (lsum x + godiag (x - 1))
in
  write (godiag (lsum 5))
end

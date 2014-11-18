let rec f = fn x => 
   (if (x = 0) then 
    0
   else
    (x + (f (x - 1))))
 val v = f
 rec f = fn x => 2
in
 write (v 10)
end

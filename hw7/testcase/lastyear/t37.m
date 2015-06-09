let val f1 = malloc (fn x => x) in
  f1 := (fn x => x + 1) ;
  (!f1) 2
end

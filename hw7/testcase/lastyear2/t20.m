let val f = fn x => x = x in
 if (f 1) then (f true) else (f "string")
end
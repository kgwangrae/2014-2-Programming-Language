let val g = fn x => x = true
    rec f = fn x => (if (x = 1) then (g x) else (f (x - 1)))
in
  (f 4)
end
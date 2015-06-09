let val f1 = fn x => x
    val f2 = fn y => y
    val p = (f1, f2)
  in
  (p.1 "true", p.2 true)
end

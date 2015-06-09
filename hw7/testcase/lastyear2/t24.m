let val f1 = fn x => x
    val p = (f1, f1)
in
  (p.1 "true", p.2 true)
end
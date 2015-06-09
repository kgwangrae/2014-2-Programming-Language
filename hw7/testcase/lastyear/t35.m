let val f1 = fn x => x
    val f2 = fn y => f1 true
    val f3 = (f1 2, f2 (malloc 2))
  in f3
end

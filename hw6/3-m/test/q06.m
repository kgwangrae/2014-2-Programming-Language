let val p = malloc (2, malloc 4) in
  write (!p).1 + !((!p).2)
end
let val x1 = malloc malloc malloc 1 in
 let val x2 = !x1 in
  let val x3 = !x2 in
   (fn x => x := 3) x3
  end
 end
end

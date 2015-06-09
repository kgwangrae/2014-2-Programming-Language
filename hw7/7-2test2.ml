[Header]
open M
open Hw7_2

let (|>) g f = f g


let test1 file =
    let ch = open_in file in
    let lexbuf = Lexing.from_channel ch in
    let pgm = Parser.program Lexer.start lexbuf in
    close_in ch;
    M_PolyChecker.check pgm

let test2 str =
    let lexbuf = Lexing.from_string str in
    let pgm = Parser.program Lexer.start lexbuf in    
    M_PolyChecker.check pgm



[Test] 
test2 "1 + 1"

[Value]
TyInt


[Test] 
test2 "1 + true"

[Exception]
 _ 

[Test] 
test2 " \
let val f = fn x => malloc x in \
  let val a = f 10 \
      val b = f \"pl\"  \
      val c = f true \
  in \
    a := !a + 1; \
    b := \"hw7\"; \
    c := !c or false \
  end \
end"

[Value]
TyBool

[Test] 
test2 " \
let val swap = (fn order_pair => \
  if (order_pair.1) (order_pair.2) \ 
    then (order_pair.2) \
    else (order_pair.2.2, order_pair.2.1)) \
in \
  swap(fn pair => pair.1 + 1 = pair.2, (1,2)); \
  swap(fn pair => pair.1 or pair.2, (true, false)) \
end"

[Value]
TyPair (TyBool, TyBool)

[Test] 
test2 "if true then 1 else true"

[Exception]
 _

[Test] 
test2 " \
let val I = fn x => x \
    val add = fn x => x.1 + x.1 \
    val const = fn n => 10 \
in \
  I I; \
  add(1, true) + add(2, \"snu 310 fall 2011\"); \
  const 1 + const true + const \"kwangkeun yi\" \
end"

[Value]
TyInt

[Test] 
test2 "write(\"asdasd\")"

[Value]
TyString

[Test] 
test2 "write(1); write(\"a\"); write(true)"

[Value]
TyBool

[Test] 
test2 "(1, true)"

[Value]
TyPair( TyInt, TyBool)

[Test] 
test2 "(1, 2).1"

[Value]
TyInt

[Test] 
test2 "(fn x => 1) 1"

[Value]
TyInt

[Test] 
test2 "(fn x => 1) true"

[Value]
TyInt

[Test] 
test2 "malloc 1"

[Value] 
TyLoc TyInt

[Test] 
test2 "malloc 1 := 2"

[Value]
TyInt

[Test] 
test2 "!malloc 1"

[Value]
TyInt

[Test] 
test2 "let val f = fn x => (x + 1) in f 2 end"

[Value]
TyInt

[Test] 
test2 "let rec f = fn x => if (1 = x) then 1 \
   else \
    if (0 = x) then  \
     1 \
    else \
     (f (x - 1) + f (x - 2)) \
in \
 f 10 \
end"

[Value]
TyInt

[Test] 
test2 "let val l = malloc (fn x => (1 + x)) in \
 (!l) 2 \
end "

[Value]
TyInt

[Test]
test2 "(fn f => fn y => (fn x => x) (f y)) (fn x => x + 1) 2"

[Value]
TyInt

[Test] 
test2 "let val f = fn x => x = x in \
 if (f 1) then (f true) else (f \"string\") \
end "

[Value]
TyBool

[Test] 
test2 "let val x1 = malloc malloc malloc 1 in \
 let val x2 = !x1 in \
  let val x3 = !x2 in \
   (fn x => x := 3) x3 \
  end \
 end \
end"

[Value]
TyInt

[Test] 
test1 "sort.m" 

[Value]
TyInt

[test] 
test2 " \
let val f = fn x => (x = 2) in \
  f true; 3 \ 
end"

[Exception]
 _

[Test] 
test2 " \
let val f = fn x => x in \
  (f f) 1; \"good\" \ 
end"

[Value]
TyString

[Test] 
test2 " \
let val f1 = fn x => x \
    val p = (f1, f1) \
in \
  (p.1 \"true\", p.2 true) \
end"

[Value]
TyPair (TyString, TyBool)

[Test] 
test2 " \
let val g = fn x => x = true \
    rec f = fn x => (if (x = 1) then (g x) else (f (x - 1))) \
in \
  (f 4) \
end"

[Exception]
 _

[Test] 
test2 " \
let val f1 = fn x => x \
    val f2 = fn y => f1 true \
    val f3 = (f1 2, f2 (malloc 2)) \
in \
  f3 \
end"

[Value]
TyPair (TyInt, TyBool)

[Test] 
test2 " \
let val f1 = fn x => x \
    val f2 = fn y => f1 true \
    val f3 = malloc (f1 false, f2 (malloc 2)) \
in \
  f3 \
end"

[Value]
TyLoc (TyPair (TyBool, TyBool))

[Test] 
test2 " \
let val p1 = (fn x => x) \
    val p2 = (fn x => x + 1) \
    val f1 = malloc p1 in \
  f1 := p2 ; \
  (!f1) 2 \
end"

[Value]
TyInt

[Test]
test2 " \
let val p1 = (fn x => x = x) \
    val p2 = (fn x => x + 1) \
    val f1 = malloc p1 in \
  f1 := p2 ; \
  (!f1) 2 \
end"

[Exception]
 _

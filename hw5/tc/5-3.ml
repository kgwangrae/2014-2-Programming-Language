[Header]

open HW
open Lambda

type nexp = NId of int
          | NLam of int * nexp
          | NApp of nexp * nexp


let normalize e = 
    let count = ref 0 in
    let newN () = count := !count + 1; !count in

    let rec iter e env = 
        match e with
        | Id str ->            
            let n = try List.assoc str env with Not_found -> newN () in
            (NId n)

        | Lam (x, e') ->
            let n = newN() in
            let ne = iter e' ((x, n)::env) in
            NLam(n, ne)

        | App (e1, e2) ->            
            NApp(iter e1 env, iter e2 env)
    in
    iter e []

let equal p1 p2 =
    (normalize p1) = (normalize p2)

let makePgm str = 
    let lexbuf = Lexing.from_string str in
    Parser.program Lexer.start lexbuf


let test inputStr expectStr =
    let pgm = makePgm inputStr in
    let expectPGM = makePgm expectStr in
    equal (HW.reduce pgm) expectPGM


[Test]
test "\\x.x" "\\x.x"

[Value]
true


[Test]
test "(\\x.x) x" "x"

[Value]
true


[Test]
test "x (\\y.y)" "x (\\y.y)"

[Value]
true


[Test]
test "(\\x.(\\y.y) z) k" "z"

[Value]
true

[Test]
test "((\\x.y) ((\\x.(x x)) (\\x.(x x))))" "y"

[Value]
true

 
[Test]
test "(\\y.(\\x.(y (((\\s.(\\z.z)) y) x))))" "\\y.(\\x.(y x))"

[Value]
true


[Test]
test "((\\x.((\\x.(\\y.((y x) x))) x)) z)" "(\\y.((y z) z))"

[Value]
true

[Test]
test "(\\y.(\\z.((\\x.(x y))(\\x.x z)))x)(\\x.x x)" "x x"

[Value]
true

[Test]
test "((\\y.((\\x.x x)((\\x.x x)((\\x.x x)y))))(\\x.x)) z" "z"

[Value]
true


[Test]
test "((\\y.((\\x.y x)((\\x.y x)((\\x.y x)y))))(\\x.x)) z" "z"

[Value]
true


[Test]
test "(\\x.(\\y.(((\\x.x) (\\x.(y x))) (\\x.x))))" "(\\x.(\\y.(y (\\x.x))))"

[Value]
true



[Test]
test "((\\y.(\\z.((\\x.(x y)) (\\x.x z))) x) (\\x.(\\y.y x))) (\\x. x)" "x"

[Value]
true



[Test]
test "((((\\x.(\\x.(\\x.((y (\\x.((z x) x))) z)))) x) y) z)" "(((y (\\x.((z x) x))) z))"

[Value]
true



[Test]
test "(((\\x.\\y.\\z.((z((x y) z))(x y)))(\\x.\\y.y))(\\x.\\x.x))(\\z.x z)" "(x (\\z.(x z))) (\\y.y)"

[Value]
true


[Test]
test "(\\a.\\b.b) ((\\x.x x) (\\x.x x))" "\\b.b"

[Value]
true

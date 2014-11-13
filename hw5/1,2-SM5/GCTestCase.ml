open Sm5
open Sm5

(* 표준 출력을 리다이렉션해서 채점 *)
(* let tester cmd value =
    let newstdout = open_out "redirected_out" in
    let _ = Unix.dup2 (Unix.descr_of_out_channel newstdout) Unix.stdout in
    let _ = run cmd in
	let _ = close_out newstdout in
    let ic = open_in "redirected_out" in
        try (
            let line = input_line ic in
	            (line = value)
        ) with _ -> false *)

(* 명령어 n 번 붙이기 *)
let append (n: int) (f: int -> command) (cmd: command) : command = 
    let rec iter i =
        if i = n then []
        else (f i) @ iter (i + 1) in
    cmd @ (iter 0)

(* 전체 테스트 목록. (total: 40 points)
 * (2 points) 01. 기본 조건: 8192개까지는 메모리 할당이 제대로 이루어져야 한다.
 * (2 points) 02. 기본 테스트: 8192개 모두 유효하게 할당되어 있는 상태에서의 메모리 할당 시도.
 ***** <01, 02 를 통과하지 못하면 아래 테스트는 모두 0점> *****
 * (2 points) 03. 쉬운 수집 대상 1개 + 유효 수집 8191개 이후 메모리 할당 시도 - gc 되어야 함.
 * (6 points) 04. 메모리 체인이 있을 때 gc 가 이를 잘 판별하는지.
 * (4 points) 05. loop 중간에 바인딩 없이 할당한 경우.
 * (4 points) 06. BOX 안의 메모리 주소는 수집하면 안됨.
 * (6 points) 07. Continuation 에 할당한 메모리: 수집하면 안 되는 경우.
 * (8 points) 08. Continuation 에 할당한 메모리: 수집해도 되는 경우.
 * (6 points) 09. 같은 변수로 바인딩 되었다고 해도 수집하면 안됨: unbind 후에 할당 가능.
 *)

let func1 i = let v = Printf.sprintf "x%d" i in [MALLOC;BIND v;PUSH (Val (Z 1));PUSH (Id v);STORE]
let func2 i = [PUSH (Id (Printf.sprintf "x%d" i));LOAD;ADD;]

let cmds = append 128 func1 []
let cmds' = append 128 func2 (cmds @ [PUSH (Val (Z 0))])
let cmds'' = cmds' @ [PUT;]
let _ = Format.printf "1st Case: output should be 128 -   "
let _ = run cmds''          (* The output should be 128 *)

(*true *)

let cmds2 = append 129 func1 []
let _ = Format.printf "2nd Case: Exception should be raise - "
let _ =
  try ((run cmds2); Format.printf "Failed\n")           (* An Exception should be raise *)
  with GC_Failure -> Format.printf "Okay\n"

(* Exception *)


(* Third case *)
let cmds3 = 
    let cmds = [
        PUSH (Val (Z 1));
        MALLOC;
        STORE;
    ] in

    let cmds = append 127 (fun i -> 
        let v = Printf.sprintf "x%d" i in [
            MALLOC; 
            BIND v; 
            PUSH (Val (Z 1));
            PUSH (Id v);
            STORE;
        ]) cmds in

    let cmds = cmds @ [
        PUSH (Val (B false));
        MALLOC;
        STORE;

        PUSH (Val (Z 0));
    ] in

    let cmds = append 127 (fun i -> 
        let v = Printf.sprintf "x%d" i in [
            PUSH (Id v);
            LOAD;
            ADD;
         ]) cmds in 

    let cmds = cmds @ [PUT] in
        cmds

let _ = Format.printf "3rd Case: output should be 127 -   "
let _ = run cmds3       (* Should print 127 *)

(* true *)


(* Fourth Case *)

let cmds4 =
    let cmds = [MALLOC;BIND "start";PUSH (Id "start");BIND "cur";] in
    let cmds = append 127 (fun _ ->
        [
            MALLOC;
            PUSH (Id "cur");
            STORE;

            PUSH (Id "cur");
            LOAD;

            UNBIND;
            POP;

            BIND "cur";
        ]) cmds in

    let cmds = cmds @ [
      MALLOC;
      BIND "xx";PUSH (Val (Z 4));
      PUSH (Id "xx");
      STORE;
      MALLOC;
      BIND "xxx";
      PUSH (Val (Z 44));
      PUSH (Id "xxx");
      STORE
     ]
     in cmds

let _ = Format.printf "4th Case: Exception should be raise - "
let _ =
  try ((run cmds4); Format.printf "Failed\n")           (* An Exception should be raise *)
  with GC_Failure -> Format.printf "Okay\n"




(* Fifth case *)

let cmds5 =
  let cmds =
    append 128 (fun i ->
        let v = Printf.sprintf "x%d" i in [
          PUSH (Val (Z 1));
          MALLOC;
          STORE;
          MALLOC;
          BIND v;
          PUSH (Val (Z 1));
          PUSH (Id v);
          STORE
        ])
      []
  in

  let cmds =
    append 128 (fun i ->
        let v = Printf.sprintf "x%d" i in [
          PUSH (Id v);
          LOAD;
          ADD;
        ])
      (cmds @ [PUSH (Val (Z 0))])
  in

  let cmds = cmds @ [PUT] in
  cmds

let _ = Format.printf "5th Case: output should be 128 -    "
let _ = run cmds5

(* Should print 128 *)

(* Sixth Case *)
let cmds6 =
  let cmds = append 127 (fun i ->
      let v = Printf.sprintf "x%d" i in [
        MALLOC;
        BIND v;

        PUSH (Val (Z i));
        PUSH (Id v);
        STORE;

        UNBIND;
      ]
    ) [] in

  let cmds = cmds @ [
      BOX 127;

      MALLOC;
      BIND "x";

      PUSH (Id "x");
      STORE;

      MALLOC;

      PUSH (Val (Z 7));
      PUT
    ] in
  cmds

let _ = Format.printf "6th Case: Exception should be raise - "
let _ =
  try ((run cmds6); Format.printf "Failed\n")           (* An Exception should be raise *)
  with GC_Failure -> Format.printf "Okay\n"

(* Should raise an exception *)


(* Seventh case *)

let cmds7 =
  let cmds = [
    PUSH (Fn ("x", [
        MALLOC;
        BIND "xx";
        PUSH (Val (Z 1));
        PUSH (Id "xx");
        STORE
      ]));

    BIND "f";
    PUSH (Id "f");
  ] in

  let cmds = append 127 (fun i ->
      let v = Printf.sprintf "x%d" i in [
        MALLOC;
        BIND v;
        PUSH (Val (Z i));
        PUSH (Id v);
        STORE
      ]) cmds in

  let cmds = cmds @ [
      PUSH (Id "f");
      PUSH (Val (Z 1));
      MALLOC;
      CALL;

      PUSH (Val (Z 7));
      PUT
    ]
  in
cmds

let _ = Format.printf "7th Case: Exception should be raise - "
let _ =
  try ((run cmds7); Format.printf "Failed\n")           (* An Exception should be raise *)
  with GC_Failure -> Format.printf "Okay\n"

(* Should give an exception *)
let cmds8 =
  let cmds = [
    PUSH (Fn ("x", [
        MALLOC;
        BIND "xx";
        PUSH (Val (Z 1));
        PUSH (Id "xx");
        STORE
      ]));

    BIND "f";
    PUSH (Id "f");
  ] in

  let cmds = append 126 (fun i ->
      let v = Printf.sprintf "x%d" i in [
        MALLOC;
        BIND v;
        PUSH (Val (Z 1));
        PUSH (Id v);
        STORE;
      ]) cmds in

  let cmds = cmds @ [
      PUSH (Id "f");
      PUSH (Val (Z 1));
      MALLOC;
      CALL;

      PUSH (Id "f");
      PUSH (Val (Z 1));
      MALLOC;
      CALL;

      PUSH (Val (Z 0));
    ] in

  let cmds = append 126 (fun i ->
      let v = Printf.sprintf "x%d" i in [
        PUSH (Id v);
        LOAD;
        ADD;
      ]) cmds in

  let cmds = cmds @ [PUT] in
  cmds

let _ = Format.printf "8th Case: output should be 126 -    "
let _ = run cmds8

(* Should print 126 *)

(* Ninth case *)


let cmds9 =
  let cmds = append 128 (fun i -> [
        MALLOC;
        BIND "x";
        PUSH (Val (Z i));
        PUSH (Id "x");
        STORE;
      ]
    ) [] in
  let cmds = cmds @ [MALLOC; PUSH (Val (Z 10)); PUT] in
  cmds

let _ = Format.printf "9th Case: Exception should be raise - "
let _ =
  try ((run cmds9); Format.printf "Failed\n")   (* An Exception should be raise *)
  with GC_Failure -> Format.printf "Okay\n"





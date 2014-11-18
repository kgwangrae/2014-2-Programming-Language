(*
 * SNU 4190.310 Programming Languages 
 *
 * Lambda Calculus
 *)

type lexp = Id of string
		  | Lam of string * lexp
		  | App of lexp * lexp

exception Error of string

(*Hogi Hwang's lambda code for test. SNUCSE, hoki444@naver.com*)
let rec makenewname bstr num clist =
        let newstring = bstr^(string_of_int num) in
                if List.exists (fun(x) -> x=newstring) clist
                then makenewname bstr (num+1) clist
                else newstring

let rec changeexp : string-> string -> lexp -> lexp
        = fun str nstr e1 ->
        match e1 with
        | Id(str2) -> if str=str2 then Id(nstr) else e1
        | Lam(str2,l) -> if str=str2 then e1 else Lam(str2,changeexp str nstr l)
        | App(l1,l2) -> App(changeexp str nstr l1,changeexp str nstr l2)

let rec alpha : lexp-> string list -> (string list*lexp)
= fun e1 clist -> match e1 with
        | Id(str2) -> (str2::[],e1)
        | Lam(str2,l) -> let result=alpha l clist in
                if List.exists (fun(x) -> x=str2) clist
                then let newname=makenewname str2 1 (List.concat([clist;(fst result)])) in
                        (newname::(fst result),Lam(newname,changeexp str2 newname (snd result)))
                else (str2::(fst result),Lam(str2,(snd result)))
        | App(l1,l2) -> let r1=alpha l1 clist in
                let r2=alpha l2 clist in
                        (List.concat([(fst r1);(fst r2)]),App((snd r1),(snd r2)))

let rec makealpha : lexp -> string list
= fun e1 -> match e1 with
        | Id(str2) -> str2::[]
        | Lam(str2,l) -> str2::makealpha l
        | App(l1,l2) ->  List.concat([makealpha l1;makealpha l2])

let rec change : string-> lexp-> lexp -> lexp
        = fun str e2 e1 ->
         match e1 with
        | Id(str2) -> if str=str2 then e2 else e1
        | Lam(str2,l) -> Lam(str2,change str e2 l)
        | App(l1,l2) -> App(change str e2 l1,change str e2 l2)

let beta : string-> lexp-> lexp -> lexp
        = fun str e2 e1 ->
          let fclist = makealpha e2 in
          let ce = alpha e1 fclist in
          let newstr = makenewname str 1 (List.concat([[str];(fst ce);fclist])) in
          let cexp= changeexp str newstr (snd ce) in
          change newstr e2 cexp

let rec inreduce : lexp -> lexp
= fun exp -> match exp with
        | Id(str) -> exp
        | Lam(str,l) -> raise(Error "code error")
        | App(l1,l2) -> match l1 with
                | Lam(str,l)-> (let result=beta str l2 l in
                        match result with
                        | Lam(_,_)-> result
                        | _-> inreduce result)
                | _-> let result1=inreduce l1 in
                        if l1=result1
                        then App(l1,reduce l2)
                        else reduce (App(result1,l2))
and reduce : lexp -> lexp
= fun exp -> match exp with
        | Id(str) -> exp
        | Lam(str,l) -> Lam(str,reduce l)
        | App(l1,l2) -> match l1 with
                | Lam(str,l)-> reduce(beta str l2 l)
                | _-> let result1=inreduce l1 in
                        if l1=result1
                        then App(l1,reduce l2)
                        else reduce (App(result1,l2))

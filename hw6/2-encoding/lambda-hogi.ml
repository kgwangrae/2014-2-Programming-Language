(*Code from Hogi Hwang, SNU CSE, 2013*)
module HW =
  struct
        exception Error of string

        let rec makenewname bstr num clist =
                let newstring = bstr^(string_of_int num) in
                        if List.exists (fun(x) -> x=newstring) clist
                        then makenewname bstr (num+1) clist
                        else newstring

        let rec changeexp : string-> string -> Lambda.lexp -> Lambda.lexp
                = fun str nstr e1 ->
                match e1 with
                | Lambda.Id(str2) -> if str=str2 then Lambda.Id(nstr) else e1
                | Lambda.Lam(str2,l) -> if str=str2 then e1 else Lambda.Lam(str2,changeexp str nstr l)
                | Lambda.App(l1,l2) -> Lambda.App(changeexp str nstr l1,changeexp str nstr l2)

        let rec alpha : Lambda.lexp-> string list -> (string list*Lambda.lexp)
        = fun e1 clist -> match e1 with
                | Lambda.Id(str2) -> (str2::[],e1)
                | Lambda.Lam(str2,l) -> let result=alpha l clist in
                        if List.exists (fun(x) -> x=str2) clist
                        then let newname=makenewname str2 1 (List.concat([clist;(fst result)])) in
                                (newname::(fst result),Lambda.Lam(newname,changeexp str2 newname (snd result)))
                        else (str2::(fst result),Lambda.Lam(str2,(snd result)))
                | Lambda.App(l1,l2) -> let r1=alpha l1 clist in
                        let r2=alpha l2 clist in
                                (List.concat([(fst r1);(fst r2)]),Lambda.App((snd r1),(snd r2)))

        let rec makealpha : Lambda.lexp -> string list
        = fun e1 -> match e1 with
                | Lambda.Id(str2) -> str2::[]
                | Lambda.Lam(str2,l) -> str2::makealpha l
                | Lambda.App(l1,l2) ->  List.concat([makealpha l1;makealpha l2])

        let rec change : string-> Lambda.lexp-> Lambda.lexp -> Lambda.lexp
                = fun str e2 e1 ->
                 match e1 with
                | Lambda.Id(str2) -> if str=str2 then e2 else e1
                | Lambda.Lam(str2,l) -> Lambda.Lam(str2,change str e2 l)
                | Lambda.App(l1,l2) -> Lambda.App(change str e2 l1,change str e2 l2)

        let beta : string-> Lambda.lexp-> Lambda.lexp -> Lambda.lexp
                = fun str e2 e1 ->
                  let fclist = makealpha e2 in
                  let ce = alpha e1 fclist in
                  let newstr = makenewname str 1 (List.concat([[str];(fst ce);fclist])) in
                  let cexp= changeexp str newstr (snd ce) in
                  change newstr e2 cexp

        let rec inreduce : Lambda.lexp -> Lambda.lexp
        = fun exp -> match exp with
                | Lambda.Id(str) -> exp
                | Lambda.Lam(str,l) -> raise(Error "code error")
                | Lambda.App(l1,l2) -> match l1 with
                        | Lambda.Lam(str,l)-> (let result=beta str l2 l in
                                match result with
                                | Lambda.Lam(_,_)-> result
                                | _-> inreduce result)
                        | _-> let result1=inreduce l1 in
                                if l1=result1
                                then Lambda.App(l1,reduce l2)
                                else reduce (Lambda.App(result1,l2))
        and reduce : Lambda.lexp -> Lambda.lexp
        = fun exp -> match exp with
                | Lambda.Id(str) -> exp
                | Lambda.Lam(str,l) -> Lambda.Lam(str,reduce l)
                | Lambda.App(l1,l2) -> match l1 with
                        | Lambda.Lam(str,l)-> reduce(beta str l2 l)
                        | _-> let result1=inreduce l1 in
                                if l1=result1
                                then Lambda.App(l1,reduce l2)
                                else reduce (Lambda.App(result1,l2))



  end

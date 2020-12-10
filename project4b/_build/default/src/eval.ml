open SmallCTypes
open EvalUtils
open TokenTypes

let rec pow x p = 
  if p = 0 then 1
  else if p = 1 then x
  else if p < 0 then 0
  else x * pow x (p - 1)

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec eval_expr env t =
  match t with
  |Int x -> Int_Val(x)
  |Bool x -> Bool_Val(x)
  |ID str -> if (List.mem_assoc str env) then List.assoc str env
              else raise (DeclareError "invalid id")
  |Add (expr1, expr2) -> let e1 = eval_expr env expr1 in
                        let int1 = match e1 with 
                        |Int_Val i -> i
                        |_ -> raise (TypeError "invalid int") in 
                        let e2 = eval_expr env expr2 in 
                        let int2 = match e2 with
                        |Int_Val i -> i 
                        |_ -> raise (TypeError "invalid int") in 
                        Int_Val(int1 + int2)

  |Sub (expr1, expr2) -> let e1 = eval_expr env expr1 in
                        let int1 = match e1 with 
                        |Int_Val i -> i
                        |_ -> raise (TypeError "invalid int") in 
                        let e2 = eval_expr env expr2 in 
                        let int2 = match e2 with
                        |Int_Val i -> i 
                        |_ -> raise (TypeError "invalid int") in 
                        Int_Val(int1 - int2)
  |Mult (expr1, expr2) -> let e1 = eval_expr env expr1 in
                        let int1 = match e1 with 
                        |Int_Val i -> i
                        |_ -> raise (TypeError "invalid int") in 
                        let e2 = eval_expr env expr2 in 
                        let int2 = match e2 with
                        |Int_Val i -> i 
                        |_ -> raise (TypeError "invalid int") in 
                        Int_Val(int1 * int2)    
  |Div (expr1, expr2) -> let e1 = eval_expr env expr1 in 
                          let int1 = match e1 with 
                          |Int_Val i -> i
                          |_ -> raise (TypeError "divide error") in   
                          let e2 = eval_expr env expr2 in
                          let int2 = match e2 with
                          |Int_Val i -> i 
                          |_ -> raise (TypeError "divide error") in 
                          if int2 = 0 then raise (DivByZeroError) else
                          let value = int1 / int2 in Int_Val(value)  
  |Pow (expr1, expr2) -> let e1 = eval_expr env expr1 in 
                          let int1 = match e1 with 
                          |Int_Val i -> i
                          |_ -> raise (TypeError "pow error") in   
                          let e2 = eval_expr env expr2 in
                          let int2 = match e2 with
                          |Int_Val i -> i 
                          |_ -> raise (TypeError "pow error") in 
                          if int2 = 0 then raise (DivByZeroError) else
                          let value = pow int1 int2 in Int_Val(value)
  |Or (expr1, expr2) -> let e1 = eval_expr env expr1 in 
                          let bl1 = match e1 with 
                          |Bool_Val i -> i
                          |_ -> raise (TypeError "or error") in   
                          let e2 = eval_expr env expr2 in
                          let bl2 = match e2 with
                          |Bool_Val i -> i 
                          |_ -> raise (TypeError "or error") in 
                          Bool_Val (bl1 || bl2)
  |And (expr1, expr2) -> let e1 = eval_expr env expr1 in 
                          let bl1 = match e1 with 
                          |Bool_Val i -> i
                          |_ -> raise (TypeError "and error") in   
                          let e2 = eval_expr env expr2 in
                          let bl2 = match e2 with
                          |Bool_Val i -> i 
                          |_ -> raise (TypeError "and error") in 
                          Bool_Val (bl1 && bl2)
  |Not (expr1) -> let e1 = eval_expr env expr1 in 
                          let bl1 = match e1 with 
                          |Bool_Val i -> i
                          |_ -> raise (TypeError "not error") in   
                          if bl1 = false then Bool_Val (true) else Bool_Val (false)
  |Greater (expr1, expr2) -> let e1 = eval_expr env expr1 in 
                          let int1 = match e1 with 
                          |Int_Val i -> i
                          |_ -> raise (TypeError "greater error") in   
                          let e2 = eval_expr env expr2 in
                          let int2 = match e2 with
                          |Int_Val i -> i 
                          |_ -> raise (TypeError "greater error") in 
                          if int1 > int2 then Bool_Val(true) else Bool_Val(false)
  |GreaterEqual (expr1, expr2) -> let e1 = eval_expr env expr1 in 
                          let int1 = match e1 with 
                          |Int_Val i -> i
                          |_ -> raise (TypeError "greaterEqual error") in   
                          let e2 = eval_expr env expr2 in
                          let int2 = match e2 with
                          |Int_Val i -> i 
                          |_ -> raise (TypeError "greaterEqual error") in 
                          if int1 >= int2 then Bool_Val(true) else Bool_Val(false)
  |Less (expr1, expr2) -> let e1 = eval_expr env expr1 in 
                          let int1 = match e1 with 
                          |Int_Val i -> i
                          |_ -> raise (TypeError "less error") in   
                          let e2 = eval_expr env expr2 in
                          let int2 = match e2 with
                          |Int_Val i -> i 
                          |_ -> raise (TypeError "less error") in 
                          if int1 < int2 then Bool_Val(true) else Bool_Val(false)
  |LessEqual (expr1, expr2) -> let e1 = eval_expr env expr1 in 
                          let int1 = match e1 with 
                          |Int_Val i -> i
                          |_ -> raise (TypeError "less equal error") in   
                          let e2 = eval_expr env expr2 in
                          let int2 = match e2 with
                          |Int_Val i -> i 
                          |_ -> raise (TypeError "less equal error") in 
                          if (int1 <= int2) then Bool_Val(true) else Bool_Val(false)
  |Equal (expr1, expr2) -> let e1 = eval_expr env expr1 in 
                          (match e1 with 
                          |Int_Val (int1) -> 
                              let e2 = eval_expr env expr2 in
                              (match e2 with 
                              |Int_Val(int2) -> Bool_Val(int1 = int2)
                              |_ -> raise (TypeError "equal error"))
                          |Bool_Val(bl1) ->
                              let e2 = eval_expr env expr2 in
                              (match e2 with
                              |Bool_Val(bl2) -> Bool_Val (bl1 = bl2)
                              |_ -> raise (TypeError "equal error")))  
  |NotEqual (expr1, expr2) -> let e1 = eval_expr env expr1 in 
                              (match e1 with 
                              |Int_Val (int1) -> 
                                  let e2 = eval_expr env expr2 in
                                  (match e2 with 
                                  |Int_Val(int2) -> Bool_Val(int1 != int2)
                                  |_ -> raise (TypeError "not equal error"))
                              |Bool_Val(bl1) ->
                                  let e2 = eval_expr env expr2 in
                                  (match e2 with
                                  |Bool_Val(bl2) -> Bool_Val (bl1 != bl2)
                                  |_ -> raise (TypeError "not equal error")))    
          
  
let rec eval_stmt env s =
  match s with 
  |NoOp -> env 
  |Seq (stmt1, stmt2) -> let env1 = eval_stmt env stmt1 in 
                          let env2 = eval_stmt env1 stmt2 in env2
  |Declare (value, str) -> 
                        if List.mem_assoc str env then 
                            raise (DeclareError "Var exists already")
                        else 
                          let newType = 
                          (match value with 
                          |Int_Type -> (str, Int_Val(0))
                          |Bool_Type -> (str, Bool_Val(false))) in 
                          let newEnv = newType::env in newEnv                  
  |Assign (str, expr) -> 
                    if List.mem_assoc str env then 
                        let value = List.assoc str env in 
                        let newType = 
                        match value with 
                        |Int_Val i -> 
                           (let tempE = eval_expr env expr in 
                           (match tempE with 
                            |Int_Val int2 -> (str, Int_Val(int2))
                            |_ -> raise (TypeError "assign int error")))
                        |Bool_Val bool -> let tempE = eval_expr env expr in 
                            match tempE with 
                            |Bool_Val bool2 -> (str, Bool_Val(bool2))
                            |_ -> raise (TypeError "bool error") in newType::env 
                    else raise (DeclareError "assign var error")
  |If (expr, stmt, stmt2) -> 
                        (let g = eval_expr env expr in 
                        match g with 
                        |Bool_Val result -> (if (result = true) then 
                        let env1 = eval_stmt env stmt in env1
                        else let env1 = eval_stmt env stmt2 in env1)
                        |_ -> raise (TypeError "if error"))
  |While (expr, stmt) ->
                        let g = eval_expr env expr in 
                        (match g with 
                        |Bool_Val result -> 
                          if result then 
                            let tempEnv = eval_stmt env stmt in 
                            eval_stmt tempEnv (While(expr, stmt))
                          else env
                        |_ -> raise (TypeError "while guard error"))

  |For (str, expr, expr2, stmt) ->  
                        let forStart = eval_expr env expr in 
                        let tempEnv = 
                        (match forStart with 
                        |Int_Val i -> eval_stmt env (Assign (str, Int(i)))
                        |_ -> raise (TypeError "for start error")) in 
                        
                        let forEnd = eval_expr env expr2 in 
                        let endFor =
                        (match forEnd with 
                        |Int_Val int -> int 
                        |_ -> raise (TypeError "For end error")) in 
                        
                        let check = eval_expr tempEnv (LessEqual(ID(str), Int(endFor))) in 
                        (match check with 
                        |Bool_Val (true) -> 
                          let tempEnv2 = eval_stmt tempEnv stmt in 
                          eval_stmt tempEnv2 (For(str, Add(ID(str), Int(1)), expr2, stmt))
                        |_ -> tempEnv)
  |Print (expr) -> let eval = eval_expr env expr in 
                    match eval with 
                    |Int_Val i ->  print_output_int i;
                                  print_output_newline();
                                  env
                    |Bool_Val b -> print_output_bool b;
                                  print_output_newline();
                                  env


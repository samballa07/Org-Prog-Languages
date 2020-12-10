open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
 | [] -> raise (InvalidInputException "No more tokens")
 | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
 | [] -> raise (InvalidInputException(string_of_token tok))
 | h::t when h = tok -> t
 | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)

let rec parse_expr toks : expr_result =
  parse_or toks 
  and parse_or toks =
    let (remaining, expr) = parse_and toks in 
    let next = lookahead remaining in 
    match next with  
     |Tok_Or -> let newTokLst = match_token remaining Tok_Or in 
                  let (remaining2, expr2) = parse_or newTokLst in 
                  (remaining2, Or(expr, expr2))
     |_ -> (remaining, expr)

  and parse_and toks = 
    let (remaining, expr) = parse_equality toks in 
    let next = lookahead remaining in 
    match next with 
   |Tok_And ->  let newTokLst = match_token remaining Tok_And in 
                  let (remaining2, expr2) = parse_and newTokLst in 
                  (remaining2, And (expr, expr2))
   |_ -> (remaining, expr)

  and parse_equality toks =
    let (remaining, expr) = parse_relational toks in 
    let next = lookahead remaining in 
    match next with 
   |Tok_Equal ->  let newTokLst = match_token remaining Tok_Equal in 
                    let (remaining2, expr2) = parse_equality newTokLst in 
                    (remaining2, Equal(expr, expr2))
   |Tok_NotEqual -> let newTokLst = match_token remaining Tok_NotEqual in 
                      let (remaining2, expr2) = parse_equality newTokLst in 
                      (remaining2, NotEqual(expr, expr2))
   |_ -> (remaining, expr)

  and parse_relational toks = 
    let (remaining, expr) = parse_add toks in 
    let next = lookahead remaining in 
    match next with 
   |Tok_Less -> let newTokLst = match_token remaining Tok_Less in 
                  let (remaining2, expr2) = parse_relational newTokLst in 
                  (remaining2, Less(expr, expr2))
   |Tok_LessEqual -> let newTokLst = match_token remaining Tok_LessEqual in 
                      let (remaining2, expr2) = parse_relational newTokLst in 
                       (remaining2, LessEqual(expr, expr2))
   |Tok_Greater -> let newTokLst = match_token remaining Tok_Greater in 
                    let (remaining2, expr2) = parse_relational newTokLst in 
                     (remaining2, Greater(expr, expr2))
   |Tok_GreaterEqual -> let newTokLst = match_token remaining Tok_GreaterEqual in 
                        let (remaining2, expr2) = parse_relational newTokLst in 
                        (remaining2, GreaterEqual(expr, expr2))
   |_ -> (remaining, expr)

  and parse_add toks =
    let (remaining, expr) = parse_mult toks in 
    let next = lookahead remaining in 
    match next with 
   |Tok_Add ->  let newTokLst = match_token remaining Tok_Add in 
                  let (remaining2, expr2) = parse_add newTokLst in 
                  (remaining2, Add(expr, expr2))
   |Tok_Sub ->  let newTokLst = match_token remaining Tok_Sub in 
                  let (remaining2, expr2) = parse_add newTokLst in 
                  (remaining2, Sub(expr, expr2))
   |_ -> (remaining, expr)

  and parse_mult toks =
    let (remaining, expr) = parse_power toks in 
    let next = lookahead remaining in 
    match next with 
   |Tok_Mult -> let newTokLst = match_token remaining Tok_Mult in 
                let (remaining2, expr2) = parse_mult newTokLst in 
                  (remaining2, Mult(expr, expr2))
   |Tok_Div ->  let newTokLst = match_token remaining Tok_Div in 
                let (remaining2, expr2) = parse_mult newTokLst in 
                (remaining2, Div(expr, expr2))
   |_ -> (remaining, expr)

  and parse_power toks =
    let (remaining, expr) = parse_unary toks in 
    let next = lookahead remaining in 
    match next with 
   |Tok_Pow ->  let newTokLst = match_token remaining Tok_Pow in 
                let (remaining2, expr2) = parse_power newTokLst in 
                (remaining2, Pow(expr, expr2))
   |_ -> (remaining, expr)

  and parse_unary toks =
    let next = lookahead toks in 
    (match next with 
   |Tok_Not ->  let newTokLst = match_token toks Tok_Not in
                let (remaining, expr) = parse_unary newTokLst in
                (remaining, Not(expr)) 
   |_ -> parse_primary toks)

  and parse_primary toks =  
    let next = lookahead toks in 
    (match next with 
   |Tok_Int int ->  let newTokLst = match_token toks (Tok_Int (int)) in 
                        (newTokLst, Int(int))
   |Tok_Bool bool -> let newTokLst = match_token toks (Tok_Bool (bool)) in 
                          (newTokLst, Bool(bool)) 
   |Tok_ID str -> let newTokLst = match_token toks (Tok_ID (str)) in 
                          (newTokLst, ID(str))
   |Tok_LParen -> let newTokLst = match_token toks Tok_LParen in 
                    let (remaining, expr) = parse_expr newTokLst in 
                    let newTokLst2 = match_token remaining Tok_RParen in 
                    (newTokLst2, expr)

   |_ -> raise (InvalidInputException "parse error")) 
  
let rec parse_stmt toks =
 match lookahead toks with
 |Tok_Int_Type -> (let toks1 = match_token toks Tok_Int_Type  in
                    let next = lookahead toks1 in 
                    match next with
                   |Tok_ID str -> let tokLst2 = match_token toks1 (Tok_ID str)  in
                                  let toks3  = match_token tokLst2 Tok_Semi  in
                                  let ( toks4, stmt) = (parse_stmt toks3) in
                                  ( toks4, Seq(Declare(Int_Type, str), stmt))
                   |_ -> raise (InvalidInputException "int type error"))


 |Tok_Bool_Type -> (let toks1 = match_token toks Tok_Bool_Type in
                      let next = lookahead toks1 in 
                      match next with
                     |Tok_ID str -> let tokLst2 = match_token toks1 (Tok_ID str)  in
                                        let toks3  = match_token tokLst2 Tok_Semi  in
                                        let ( toks4, stmt) = (parse_stmt toks3) in
                                        (toks4, Seq(Declare(Bool_Type, str), stmt))

                     |_ -> raise (InvalidInputException "booltype error"))


 |Tok_ID str -> (let toks1 = match_token toks (Tok_ID str) in
                      let next = lookahead toks1 in 
                      match next with
                     |Tok_Assign -> let tokLst2 = (match_token toks1 Tok_Assign) in
                                      let (toks3, expr) = (parse_expr tokLst2) in
                                      let toks4 = match_token toks3  Tok_Semi in
                                      let (toks5, stmt) = parse_stmt toks4 in
                                      (toks5, Seq(Assign(str, expr), stmt))
                     |_ -> raise (InvalidInputException "ID error"))

 |Tok_Print -> (let toks1 = match_token toks Tok_Print in
                  let next = lookahead toks1 in 
                  match next with
                 |Tok_LParen -> let tokLst2 = match_token toks1 Tok_LParen in
                                  let (toks3, expr) = parse_expr tokLst2 in
                                  let toks4 = match_token toks3  Tok_RParen in
                                  let toks5 = match_token toks4 Tok_Semi in
                                  let (toks6, stmt) = (parse_stmt toks5) in
                                  (toks6, Seq(Print(expr), stmt))
                   |_ -> raise (InvalidInputException "print error"))

 |Tok_If -> (let toks1 = match_token toks Tok_If in
              let next = lookahead toks1 in 
              match next with
             |Tok_LParen -> let toks2 = match_token toks1 Tok_LParen in
                              let (toks3, expr) = parse_expr toks2 in
                              let toks4 = match_token toks3  Tok_RParen in
                              let toks5 = match_token toks4 Tok_LBrace in
                              let (toks6, stmt) = (parse_stmt toks5) in
                              let toks6 = match_token toks6 Tok_RBrace in
                              (match lookahead toks6 with
                             |Tok_Else -> let toks7 = match_token toks6 Tok_Else in
                                            let toks8 = match_token toks7 Tok_LBrace in
                                            let (toks9, stmt2) = parse_stmt toks8 in
                                            let toks10 = match_token toks9 Tok_RBrace in
                                            let (toks11, stmt3) = parse_stmt toks10 in
                                            (toks11, Seq(If(expr, stmt, stmt2), stmt3))

                             |_ -> let (toks7, stmt2) = parse_stmt toks6 in
                                      (toks7, Seq (If(expr, stmt, NoOp), stmt2)))

             |_  -> raise (InvalidInputException "if error"))
  
 |Tok_For -> (let toks1 = match_token toks Tok_For in 
                let next = lookahead toks1 in 
                (match next with 
               |Tok_LParen -> let tokLst2 = match_token toks1 Tok_LParen in 
                                let next2 = lookahead tokLst2 in 
                                (match next2 with 
                               |Tok_ID str -> let toks3  = match_token tokLst2 (Tok_ID str) in 
                                                   let toks4 = match_token toks3  Tok_From in 
                                                   let (toks5, expr) = parse_expr toks4 in 
                                                   let toks6 = match_token toks5 Tok_To in 
                                                   let (toks7, expr2) = parse_expr toks6 in 
                                                   let toks8 = match_token toks7 Tok_RParen in 
                                                   let toks9 = match_token toks8 Tok_LBrace in 
                                                   let (toks10, stmt) = parse_stmt toks9 in 
                                                   let toks11 = match_token toks10 Tok_RBrace in 
                                                   let (toks12, stmt2) = parse_stmt toks11 in 
                                                   (toks12, Seq (For (str, expr, expr2, stmt), stmt2))
                               |_ -> raise (InvalidInputException "id error for For"))
               |_ -> raise (InvalidInputException "for error")))

 |Tok_While ->  (let toks1 = match_token toks Tok_While in
                  let next = lookahead toks1 in 
                   match next with
                 |Tok_LParen -> let tokLst2 = match_token toks1 Tok_LParen in
                                  let (toks3, expr) = parse_expr tokLst2 in
                                  let toks4 = match_token toks3  Tok_RParen in
                                  let toks5 = match_token toks4 Tok_LBrace in
                                  let (toks6, stmt) = parse_stmt toks5 in
                                  let toks7 = match_token toks6 Tok_RBrace in
                                  let (toks8, stmt2) = parse_stmt toks7 in
                                  (toks8, Seq(While(expr, stmt), stmt2))
                 |_ -> raise (InvalidInputException "while error"))
  |Tok_RBrace -> (toks, NoOp)
  |EOF -> (toks, NoOp)
  |_ -> raise (InvalidInputException "parse error")

  let parse_main toks =

    let toks1 = match_token toks Tok_Int_Type in
    let toks2 = match_token toks1 Tok_Main in
    let toks3 = match_token toks2 Tok_LParen in
    let toks4 = match_token toks3 Tok_RParen in
    let toks5 = match_token toks4 Tok_LBrace in
    let (toks6, stmt) = parse_stmt toks5 in
    stmt
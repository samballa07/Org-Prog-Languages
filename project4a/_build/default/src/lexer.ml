open TokenTypes

 let tokenize input =
  let rec tok pos = 
      if pos >= String.length input then [EOF]
      else if (Str.string_match (Str.regexp ("-?[0-9]+")) input pos) then
          let matched_int = Str.matched_string input in 
          Tok_Int (int_of_string matched_int)::(tok (pos + (String.length matched_int)))
      else if (Str.string_match (Str.regexp ("[a-zA-Z][a-zA-Z0-9]*")) input pos) then
        let matched_string = Str.matched_string input in 
        if Str.string_match (Str.regexp ("^while$")) matched_string 0 then 
            Tok_While::(tok (pos+5))
        else if Str.string_match (Str.regexp("^for$")) matched_string 0 then 
            Tok_For::(tok(pos+3))
        else if Str.string_match (Str.regexp("^from$")) matched_string 0 then 
            Tok_From::(tok(pos+4))
        else if Str.string_match (Str.regexp("^to$")) matched_string 0 then 
            Tok_To::(tok (pos+2))
        else if Str.string_match (Str.regexp("^int$")) matched_string 0 then 
            Tok_Int_Type::(tok(pos+3))
        else if Str.string_match (Str.regexp("^bool$")) matched_string 0 then 
            Tok_Bool_Type::(tok (pos+4))
        else if Str.string_match (Str.regexp("^printf$")) matched_string 0 then 
            Tok_Print::(tok(pos+6))
        else if Str.string_match (Str.regexp("^main$")) matched_string 0 then
            Tok_Main::(tok(pos+4))
        else if Str.string_match (Str.regexp("^else$")) matched_string 0 then 
            Tok_Else::(tok(pos+4))
        else if Str.string_match (Str.regexp("^true$")) matched_string 0 then 
            Tok_Bool(true)::(tok(pos+4))
        else if Str.string_match (Str.regexp("^false$")) matched_string 0 then 
            Tok_Bool(false)::(tok(pos+5))
        else if Str.string_match (Str.regexp("^if$")) matched_string 0 then 
            Tok_If ::(tok(pos+2))
        else Tok_ID(matched_string)::tok(pos + (String.length matched_string))
      else if (Str.string_match (Str.regexp "(") input pos) then 
          Tok_LParen::(tok (pos+1))
      else if (Str.string_match (Str.regexp ")") input pos) then 
          Tok_RParen::(tok (pos+1))
      else if (Str.string_match (Str.regexp "{") input pos) then 
          Tok_LBrace::(tok (pos+1))
      else if (Str.string_match (Str.regexp "}") input pos) then 
          Tok_RBrace::(tok (pos+1))
      else if (Str.string_match (Str.regexp "==") input pos) then 
          Tok_Equal::(tok (pos+2))
      else if (Str.string_match (Str.regexp "!=") input pos) then
          Tok_NotEqual::(tok (pos+2))
      else if (Str.string_match (Str.regexp "=") input pos) then
          Tok_Assign::(tok (pos+1))
      else if (Str.string_match (Str.regexp ">") input pos) then 
          Tok_Greater::(tok (pos+1))
      else if (Str.string_match (Str.regexp "<") input pos) then 
          Tok_Less::(tok (pos+1))
      else if (Str.string_match (Str.regexp ">=") input pos) then 
          Tok_GreaterEqual::(tok (pos+2))
      else if (Str.string_match (Str.regexp "<=") input pos) then 
          Tok_LessEqual::(tok (pos+2))
      else if (Str.string_match (Str.regexp "||") input pos) then 
          Tok_Or::(tok (pos+2))
      else if (Str.string_match (Str.regexp "&&") input pos) then 
          Tok_And::(tok (pos+2))
      else if (Str.string_match (Str.regexp "!") input pos) then 
          Tok_Not::(tok (pos+1))
      else if (Str.string_match (Str.regexp ";") input pos) then
          Tok_Semi::(tok (pos+1))
      else if (Str.string_match (Str.regexp "\\+") input pos) then 
          Tok_Add::(tok (pos+1))
      else if (Str.string_match (Str.regexp "-") input pos) then
          Tok_Sub::(tok (pos+1))
      else if (Str.string_match (Str.regexp "\\*") input pos) then
          Tok_Mult::(tok (pos+1))
      else if (Str.string_match (Str.regexp "/") input pos) then
          Tok_Div::(tok (pos+1))
      else if (Str.string_match (Str.regexp "\\^") input pos) then
          Tok_Pow::(tok (pos+1))
      else (tok (pos+1)) in tok 0 
  
  
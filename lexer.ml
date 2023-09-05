open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)
let z_lp = Str.regexp "(" 
let z_rp = Str.regexp ")" 
let z_eql = Str.regexp "=" 
let z_noteql = Str.regexp "<>" 
let z_bigger = Str.regexp ">" 
let z_smaller = Str.regexp "<" 
let z_bigeql = Str.regexp ">=" 
let z_smalleql = Str.regexp "<=" 
let z_or = Str.regexp "||" 
let z_and = Str.regexp "&&" 
let z_not = Str.regexp "not " 
let z_if = Str.regexp "if " 
let z_then = Str.regexp "then " 
let z_else = Str.regexp "else " 
let z_add = Str.regexp "\\+" 
let z_sub = Str.regexp "-" 
let z_mult = Str.regexp "\\*" 
let z_div = Str.regexp "/" 
let z_concat = Str.regexp "\\^" 
let z_let = Str.regexp "let " 
let z_def = Str.regexp "def " 
let z_in = Str.regexp "in " 
let z_rec = Str.regexp "rec " 
let z_fun = Str.regexp "fun " 
let z_arrow = Str.regexp "->" 
let z_doublesemi = Str.regexp ";;" 
let z_booltrue = Str.regexp "true" 
let z_boolfalse = Str.regexp "false"
let z_intpos = Str.regexp "[0-9]+"
let z_intneg = Str.regexp "(-[0-9]+)"
let z_string = Str.regexp "\"[^\"]*\"" 
let z_ID = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" 
    
    let tokenize input = 
        let rec ztok pos s =
            if pos >= String.length s then []

            else if Str.string_match (Str.regexp " ") s pos then
                ztok (pos+1) s

            else if (Str.string_match z_booltrue s pos) then       
                (Tok_Bool true)::(ztok (pos+4) s) 
            else if (Str.string_match z_boolfalse s pos) then      
                (Tok_Bool false)::(ztok (pos+5) s) 

            else if (Str.string_match z_string s pos) then       
                let stringtoken = Str.matched_string s in 
                    let sanitized = String.sub stringtoken 1 ((String.length stringtoken)-2) in
                        (Tok_String (sanitized))::(ztok (pos+(String.length stringtoken)) s) 
            
            else if (Str.string_match z_intpos s pos) then       
                let inttoken = Str.matched_string s in        
                    (Tok_Int (int_of_string inttoken))::(ztok (pos+(String.length inttoken)) s) 
            
            else if (Str.string_match z_intneg s pos) then
                let inttoken = Str.matched_string s in     
                    let valueneg = String.sub inttoken 1 ((String.length inttoken)-2) in  
                        (Tok_Int (int_of_string valueneg))::(ztok (pos+(String.length inttoken)) s) 
            
            else if (Str.string_match z_let s pos) then 
                Tok_Let::(ztok (pos+3) s)    
            else if (Str.string_match z_def s pos) then       
                Tok_Def::(ztok (pos+3) s)  
            else if (Str.string_match z_in s pos) then       
                Tok_In::(ztok (pos+2) s)  
            else if (Str.string_match z_rec s pos) then       
                Tok_Rec::(ztok (pos+3) s)  
            else if (Str.string_match z_fun s pos) then       
                Tok_Fun::(ztok (pos+3) s)  
            else if (Str.string_match z_not s pos) then       
                Tok_Not::(ztok (pos+3) s)  
            else if (Str.string_match z_if s pos) then       
                Tok_If::(ztok (pos+2) s)  
            else if (Str.string_match z_then s pos) then       
                Tok_Then::(ztok (pos+4) s)  
            else if (Str.string_match z_else s pos) then       
                Tok_Else::(ztok (pos+4) s)  

            else if (Str.string_match z_ID s pos) then       
                let idtoken = Str.matched_string s in 
                    (Tok_ID idtoken)::(ztok (pos+ (String.length idtoken)) s)
            
            else if (Str.string_match z_arrow s pos) then       
                Tok_Arrow::(ztok (pos+2) s)  

            else if (Str.string_match z_noteql s pos) then       
                Tok_NotEqual::(ztok (pos+2) s)  
            else if (Str.string_match z_bigeql s pos) then       
                Tok_GreaterEqual::(ztok (pos+2) s)  
            else if (Str.string_match z_smalleql s pos) then       
                Tok_LessEqual::(ztok (pos+2) s)  
            else if (Str.string_match z_or s pos) then       
                Tok_Or::(ztok (pos+2) s)  
            else if (Str.string_match z_and s pos) then       
                Tok_And::(ztok (pos+2) s) 
            else if (Str.string_match z_doublesemi s pos) then       
                Tok_DoubleSemi::(ztok (pos+2) s)  
            else if (Str.string_match z_eql s pos) then       
                Tok_Equal::(ztok (pos+1) s)   
            else if (Str.string_match z_bigger s pos) then       
                Tok_Greater::(ztok (pos+1) s)  
            else if (Str.string_match z_smaller s pos) then       
                Tok_Less::(ztok (pos+1) s)  


            else if (Str.string_match z_add s pos) then       
                Tok_Add::(ztok (pos+1) s)  
            else if (Str.string_match z_sub s pos) then       
                Tok_Sub::(ztok (pos+1) s)  
            else if (Str.string_match z_mult s pos) then       
                Tok_Mult::(ztok (pos+1) s)  
            else if (Str.string_match z_div s pos) then       
                Tok_Div::(ztok (pos+1) s)  
            else if (Str.string_match z_concat s pos) then       
                Tok_Concat::(ztok (pos+1) s)   

            else if (Str.string_match z_lp s pos) then     
                Tok_LParen::(ztok (pos+1) s)     
            else if (Str.string_match z_rp s pos) then       
                Tok_RParen::(ztok (pos+1) s)  

            else raise (InvalidInputException "NO!")
                in ztok 0 input;;


    

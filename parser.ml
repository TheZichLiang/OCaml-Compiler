open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
  match (lookahead toks) with
  | Some (Tok_Let) -> parse_let toks
  | Some (Tok_If) -> parse_if toks
  | Some (Tok_Fun)-> parse_fun toks
  | _ -> parse_or toks

and parse_let toks = 
   let t = match_token toks Tok_Let in
      match (lookahead t) with
        | Some (Tok_Rec) -> let reckey = match_token t Tok_Rec in 
                                (match lookahead reckey with
                                  | Some (Tok_ID x) ->  let ttt = match_token reckey (Tok_ID x) in
                                                          let tttt = match_token ttt Tok_Equal in
                                                            let (p,q) = parse_expr tttt in 
                                                              let ttttt = match_token p Tok_In in
                                                                let (r,s) = parse_expr ttttt in 
                                                                  (r,(Let (x, true, q,s)))
                                  | _ -> raise (InvalidInputException "Invalid keyword"))
        
        | Some (Tok_ID x) -> let ttt = match_token t (Tok_ID x) in
                              let tttt = match_token ttt Tok_Equal in
                                let (p,q) = parse_expr tttt in 
                                   let ttttt = match_token p Tok_In in
                                      let (r,s) = parse_expr ttttt in (r, (Let (x, false, q,s)))
        
        | _ -> raise (InvalidInputException "Parse_let failed")

and parse_if toks = 
  let t = match_token toks Tok_If in
    let (p,q) = parse_expr t in 
      let tt = match_token p Tok_Then in
        let (r,s) = parse_expr tt in 
          let ttt = match_token r Tok_Else in
            let (x,y) = parse_expr ttt in (x, (If (q,s,y)))           
 
and parse_fun toks =
    let t = match_token toks Tok_Fun in
      let extractedvalue = (match (lookahead t) with 
                            | Some (Tok_ID a) -> a
                            | _ ->  raise (InvalidInputException "Invalid keyword")) in
      let tt = match_token t (Tok_ID extractedvalue) in
          let ttt = match_token tt Tok_Arrow in
            let (x,y) = parse_expr ttt in (x, (Fun (extractedvalue, y)))

and parse_or toks = 
  let (t, a) = parse_and toks in
    match lookahead t with
    | Some (Tok_Or) -> let tt = match_token t Tok_Or in
                let (ttt, b) = parse_or tt in
                (ttt, Binop (Or, a, b))
    | _ -> t, a

and parse_and toks = 
  let (t, a) = parse_equal toks in
    match lookahead t with
    | Some (Tok_And) -> let tt = match_token t Tok_And in
                let (ttt, b) = parse_and tt in
                (ttt, Binop (And, a, b))
    | _ -> t, a

and parse_equal toks =
  let (t, a) = parse_reltn toks in
    match lookahead t with
    | Some (Tok_Equal) -> let tt = match_token t Tok_Equal in
                let (ttt, b) = parse_equal tt in
                (ttt, Binop (Equal, a, b))
    | Some (Tok_NotEqual) -> let tt = match_token t Tok_NotEqual in
                let (ttt, b) = parse_equal tt in
                (ttt, Binop (NotEqual, a, b))
    | _ -> t, a

and parse_reltn toks =
  let (t, a) = parse_add toks in
    match lookahead t with
    | Some (Tok_Less) -> let tt = match_token t Tok_Less in
                          let (ttt, b) = parse_reltn tt in
                            (ttt, Binop (Less, a, b)) 
    | Some (Tok_Greater) -> let tt = match_token t Tok_Greater in
                              let (ttt, b) = parse_reltn tt in
                                (ttt, Binop (Greater, a, b)) 
    | Some (Tok_LessEqual) -> let tt = match_token t Tok_LessEqual in
                                let (ttt, b) = parse_reltn tt in
                                  (ttt, Binop (LessEqual, a, b)) 
    | Some (Tok_GreaterEqual) -> let tt = match_token t Tok_GreaterEqual in
                                  let (ttt, b) = parse_reltn tt in
                                   (ttt, Binop (GreaterEqual, a, b)) 
    | _ -> t, a

and parse_add toks = 
  let (t, a) = parse_mult toks in
    match lookahead t with
    | Some (Tok_Add) -> let tt = match_token t Tok_Add in
                          let (ttt, b) = parse_add tt in
                            (ttt, Binop (Add, a, b))
    | Some (Tok_Sub) -> let tt = match_token t Tok_Sub in
                          let (ttt, b) = parse_add tt in
                            (ttt, Binop (Sub, a, b))
    | _ -> t, a

and parse_mult toks = 
  let (t, a) = parse_concat toks in
    match lookahead t with
    | Some (Tok_Mult) -> let tt = match_token t Tok_Mult in
                          let (ttt, b) = parse_mult tt in
                            (ttt, Binop (Mult, a, b))
    | Some (Tok_Div) -> let tt = match_token t Tok_Div in
                          let (ttt, b) = parse_mult tt in
                            (ttt, Binop (Div, a, b))
    | _ -> t, a

and parse_concat toks = 
  let (t, a) = parse_unary toks in
    match lookahead t with
    | Some (Tok_Concat) -> let tt = match_token t Tok_Concat in
                            let (ttt, b) = parse_concat tt in
                            (ttt, Binop (Concat, a, b))
    | _ -> t, a

and parse_unary toks = 
    match (lookahead toks) with
    | Some (Tok_Not) -> let t = match_token toks Tok_Not in
                          let (tt, a) =  parse_unary t in (tt, Not (a))
    | _ -> parse_func toks

and parse_func toks = 
  let (t, a) = parse_prim toks in
    match (lookahead t) with
      | Some (Tok_Int x) -> let (tt, b) = parse_prim t in (tt, FunctionCall(a,b))
      | Some (Tok_Bool x) -> let (tt, b) = parse_prim t in (tt, FunctionCall(a,b))
      | Some (Tok_String x) -> let (tt, b) = parse_prim t in (tt, FunctionCall(a,b))
      | Some (Tok_ID x) -> let (tt, b) = parse_prim t in (tt, FunctionCall(a,b))
      | Some (Tok_LParen) -> let (tt, b) = parse_prim t in (tt, FunctionCall(a,b))
      | _ -> t, a
    
and parse_prim toks =
    match (lookahead toks) with
    | Some (Tok_Int x) -> let t = match_token toks (Tok_Int x) in
                            (t, Value (Int x))
    | Some (Tok_Bool x) -> let t = match_token toks (Tok_Bool x) in
                            (t, Value (Bool x))
    | Some (Tok_String x) -> let t = match_token toks (Tok_String x) in
                            (t, Value (String x))
    | Some (Tok_ID x) -> let t = match_token toks (Tok_ID x) in
                            (t, ID x)
    | Some (Tok_LParen) -> let t = match_token toks Tok_LParen in
                            let (tt, b) = parse_expr t in
                              let ttt = match_token tt Tok_RParen in
                                (ttt, b)      
    | _ -> raise (InvalidInputException "pars_prim failed")   

let rec parse_mutop toks = 
  match lookahead toks with 
    | Some Tok_DoubleSemi -> let t = match_token toks Tok_DoubleSemi in (t, NoOp)
    | Some Tok_Def -> parse_def toks
    | _ -> parse_expr_mutop toks

  and parse_def toks =
    let t = match_token toks Tok_Def in
      match (lookahead t) with 
        | Some (Tok_ID a) -> let tt = match_token t (Tok_ID a) in
                                let ttt = match_token tt Tok_Equal in
                                  let (x,y) = parse_expr ttt in let tttt =      
                                    match_token x Tok_DoubleSemi in
                                      (tttt, (Def (a, y)))
        | _ ->  raise (InvalidInputException "Invalid keyword")
                              
  and parse_expr_mutop toks =
    let (t,a) = parse_expr toks in let tt = match_token t Tok_DoubleSemi in (tt, Expr(a))

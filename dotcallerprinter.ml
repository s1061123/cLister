open Unix 
open Cil
open Cabs
open Cprint

let print_string s = if s <> "" then Pervasives.print_string s else ()
let list_add l elem = if (List.exists (fun x -> x = elem) l) then l
else elem::l

let str_concat sep list = 
  String.concat sep (List.filter (fun x -> (String.compare "" x)!=0) list)

(* list the function *)
class dotCallerPrinterClass = object (self)

  val mutable cfunc: string = ""
  val mutable ngfunc: string list = []

  method type2str = function 
      Tvoid -> "void"
    | Tchar -> "char"
    | Tshort -> "short"
    | Tint -> "int"
    | Tlong -> "long"
    | Tint64 -> "int64"
    | Tfloat -> "float"
    | Tdouble -> "double"
    | Tsigned -> "signed"
    | Tunsigned -> "unsigned"
    | Tnamed(s) -> s (* "named("^s^")" *)
    | _ -> "????" 

  method spec2str = function SpecTypedef -> "typedef"
    | SpecCV cs -> "const/volatile"
    | SpecAttr at -> "attribute"
    | SpecStorage st -> "storage"
    | SpecInline -> "inline"
    | SpecType  t -> self#type2str t
    | SpecPattern s -> s
	  
  method attr2str_1 (s, x) =
    s ^ " " ^ (str_concat " " (List.map (fun e -> (self#exp2str e)) x))

  method attr2str (name, args) = 
    if args = [] then name
    else 
      begin
	str_concat "" [ name; "("; 
			if name = "__attribute__" then "(" else "" ;
			(match args with
			  [VARIABLE "aconst"] -> "const"
			| [VARIABLE "restrict"] -> "restrict"
			| _ -> str_concat ", " 
			      (List.map (fun e -> self#exp2str e) args));
			")"; 
			if name = "__attribute__" then ")" else "" ]
      end
	
  method attrs2str al = str_concat " " (List.map self#attr2str al)

  method decl2str (n:string) = function
      JUSTBASE -> if n <> "___missing_field_name" then n
      else "?"
    | PARENTYPE(al1,d,al2) ->
	"(" ^ self#attrs2str al1 ^ " " ^ 
	(self#decl2str n d) ^ " " ^
	self#attrs2str al2 ^ ")"
    | PTR(al, d) -> "*" ^ self#attrs2str al ^ "" ^ self#decl2str n d
    | ARRAY(d,al,e) -> (self#decl2str n d) ^ "[" ^ (self#attrs2str al) ^
	(if e <> NOTHING then self#exp2str e else  "") ^ "]"
    | PROTO(d,args,isva) -> (self#decl2str n d) 

  method decl2namestr (n:string) = function
      PROTO(d,args,isva) -> Some(self#decl2str n d)
    | _ -> None
	  

  method exp2str = function NOTHING -> ""
    | UNARY (u, e) -> self#exp2str e
    | LABELADDR(s) -> ""
    | BINARY(bo, e1, e2) -> str_concat "\n" [self#exp2str e1; self#exp2str e2] 
    | QUESTION(e1, e2, e3) -> str_concat "\n" [self#exp2str e1; self#exp2str e2;
					       self#exp2str e3]  
    | CAST((s,d), ie) -> ""
    | CALL(e, el) -> let fn = self#exp2str e in
      if (List.exists (fun x -> x = fn) ngfunc) then ""
      else (cfunc ^ " -> " ^  fn ^ ";")
    | COMMA(el) -> str_concat "\n" (List.map self#exp2str el)
    | CONSTANT(c) -> ""
    | PAREN(e) -> self#exp2str e
    | VARIABLE(s) -> s
    | EXPR_SIZEOF(e) -> self#exp2str e
    | TYPE_SIZEOF(s,d) -> ""
    | EXPR_ALIGNOF(e) -> self#exp2str e 
    | TYPE_ALIGNOF(s,d) -> ""
    | INDEX(e1,e2) ->  str_concat "\n" [self#exp2str e1; self#exp2str e2]
    | MEMBEROF(e, s) -> self#exp2str e
    | MEMBEROFPTR(e, s) -> self#exp2str e
    | GNU_BODY(b) -> self#block2str b
    | EXPR_PATTERN(s) -> ""	

  method const2str = function CONST_INT(s) -> s
    | CONST_FLOAT(s) -> s
    | CONST_CHAR(il) -> "CHAR"
    | CONST_WCHAR(il) -> "WCHAR"
    | CONST_STRING(s) -> s
    | CONST_WSTRING(il) -> "WSTRING"
	  
  method name2str (n,decl,attrs, _) = 
    (self#decl2str n decl)

  method name2declstr (n,decl,attrs, _) = (self#decl2namestr n decl) 

  method singlen2str ((spec:spec_elem list), (n:name)) = 
    let sl = List.map (fun x -> self#spec2str x) spec in
    if (List.exists  
          (function "storage" -> true
            | "inline" -> (ngfunc <- list_add ngfunc (self#name2str n)
                  ;true) 
            | _ -> false) sl) then
      "" 
    else (str_concat " " sl) ^ " "^ (self#name2str n)

  method stat2str = function
      COMPUTATION(exp, _) -> self#exp2str exp
    | BLOCK(b, _) -> self#block2str b
    | SEQUENCE(s1, s2, _) -> 
	str_concat "\n" [(self#stat2str s1); (self#stat2str s2)]
    | IF(exp,s1,s2,_) -> str_concat "\n" [self#exp2str exp ; self#stat2str s1; 
					  self#stat2str s2]
    | WHILE(exp, stat, _) -> str_concat "\n" [self#exp2str exp ; 
					      self#stat2str stat]
    | DOWHILE(exp, stat, _) -> str_concat "\n" [self#exp2str exp ; 
						self#stat2str stat]
    | FOR(fc1, exp2, exp3, stat, _) -> str_concat "\n"
	  (List.concat
	     [match fc1 with 
	       FC_EXP e1 -> [self#exp2str e1]
	     | FC_DECL d1 -> [self#defs2str d1]
	     | _ -> [];
		 [self#exp2str exp2];
		 [self#exp2str exp3];
		 [self#stat2str stat]
	    ])
    | RETURN(e, _) -> self#exp2str e
    | SWITCH(exp,stat, _) -> str_concat "\n" [self#exp2str exp; 
					      self#stat2str stat]
    | CASE(exp, stat, _) -> str_concat "\n" [self#exp2str exp; 
					     self#stat2str stat]
    | CASERANGE(expl, exph, stat, _) -> str_concat "\n" [self#exp2str expl; 
							 self#exp2str exph; 
							 self#stat2str stat]
    | DEFAULT(stat, _) -> self#stat2str stat
    | LABEL(_, stat, _) -> self#stat2str stat
    | COMPGOTO(exp,_) -> self#exp2str exp
    | DEFINITION d -> self#defs2str d
    | TRY_FINALLY(b, h, _) -> str_concat "\n" [ self#block2str b; self#block2str h]
    | TRY_EXCEPT(b, e, h, _) -> str_concat "\n" 
	  [ self#block2str b; self#exp2str e; self#block2str h]
    | _ -> ""

  method block2str blk = 
    str_concat "\n" (List.map self#stat2str blk.bstmts)
      
  method defs2str = function 
      FUNDEF(s, b, _, _) -> 
	(match ((fun (sl, n) -> self#name2declstr n) s) with
	  Some x -> (cfunc <- x;
                     self#singlen2str s;
		     (let 
			 ans = str_concat ";\n" [x; self#block2str b]
		     in
		     cfunc <- "" ; ans ))
	| None -> "")
    | DECDEF(ig, _) -> ""
    | TYPEDEF(ng,_) -> ""
    | ONLYTYPEDEF(spec, _) -> ""
    | GLOBASM(s, _) -> ""
    | PRAGMA(e, _) -> "PRAGMA"
    | LINKAGE(s, _, dl) -> ""
    | TRANSFORMER(d, dl, _) -> "TRANSFORMER"
    | EXPRTRANSFORMER(e1, e2, _) -> "EXPRTRANSFORMER"

  method parse (cab:definition list) = 
    begin 
      print_string ((str_concat "\n" (List.map self#defs2str cab)) ^ "\n")
    end
end (* end class *)




open Unix 
open Cil
open Cabs
open Cprint

let print_string s = if s <> "" then Pervasives.print_string s else ()

(* list the function *)
class calleePrinterClass f_name = object (self)

	val mutable inFunc: bool = false

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
	s ^ " " ^ (String.concat " " (List.map (fun e -> (self#exp2str e)) x))

	method attr2str (name, args) = 
	  if args = [] then name
	  else 
	    begin
	    String.concat "" [ name; "("; 
	    if name = "__attribute__" then "(" else "" ;
	    (match args with
		  [VARIABLE "aconst"] -> "const"
		| [VARIABLE "restrict"] -> "restrict"
		| _ -> String.concat ", " 
			(List.map (fun e -> self#exp2str e) args));
	    ")"; 
	    if name = "__attribute__" then ")" else "" ]
	    end

	method attrs2str al = String.concat " " (List.map self#attr2str al)

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
	| PROTO(d,args,isva) -> (self#decl2str n d) ^ "(" ^ 
		(String.concat ", " (List.map self#singlen2str args)) ^ 
		(if isva then ", ..."
		else "") ^ ")"

	method decl2namestr (n:string) = function
	 PROTO(d,args,isva) -> Some(self#decl2str n d)
	| _ -> None
	

	method exp2str = function NOTHING -> ""
	| UNARY (u, e) -> self#exp2str e
	| LABELADDR(s) -> ""
	| BINARY(bo, e1, e2) -> String.concat "\n" [self#exp2str e1; self#exp2str e2] 
	| QUESTION(e1, e2, e3) ->String.concat "\n" [self#exp2str e1; self#exp2str e2;
				self#exp2str e3]  
	| CAST((s,d), ie) -> ""
	| CALL(e, el) -> if (self#exp2str e) = f_name then (inFunc <- true; f_name)
			else ""
	| COMMA(el) -> String.concat "\n" (List.map self#exp2str el)
	| CONSTANT(c) -> ""
	| PAREN(e) -> self#exp2str e
	| VARIABLE(s) -> s
	| EXPR_SIZEOF(e) -> self#exp2str e
	| TYPE_SIZEOF(s,d) -> ""
	| EXPR_ALIGNOF(e) -> self#exp2str e 
	| TYPE_ALIGNOF(s,d) -> ""
	| INDEX(e1,e2) ->  String.concat "\n" [self#exp2str e1; self#exp2str e2]
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
	(self#decl2str n decl) ^ "" ^ self#attrs2str attrs

	method name2declstr (n,decl,attrs, _) = (self#decl2namestr n decl) 

	method singlen2str ((spec:spec_elem list), (n:name)) = 
	let sl = List.map (fun x -> self#spec2str x) spec in
	(String.concat " " sl) ^ " "^ (self#name2str n)

	method stat2str = function
	  COMPUTATION(exp, _) -> self#exp2str exp
	| BLOCK(b, _) -> self#block2str b
	| SEQUENCE(s1, s2, _) -> 
		String.concat "\n" [(self#stat2str s1); (self#stat2str s2)]
	| IF(exp,s1,s2,_) -> String.concat "\n" [self#exp2str exp ; self#stat2str s1; 
						self#stat2str s2]
	| WHILE(exp, stat, _) ->String.concat "\n" [self#exp2str exp ; 
						self#stat2str stat]
	| DOWHILE(exp, stat, _) ->String.concat "\n" [self#exp2str exp ; 
						self#stat2str stat]
	| FOR(fc1, exp2, exp3, stat, _) -> String.concat "\n"
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
	| SWITCH(exp,stat, _) -> String.concat "\n" [self#exp2str exp; 
				self#stat2str stat]
	| CASE(exp, stat, _) -> String.concat "\n" [self#exp2str exp; 
				self#stat2str stat]
	| CASERANGE(expl, exph, stat, _) -> String.concat "\n" [self#exp2str expl; 
				self#exp2str exph; 
				self#stat2str stat]
	| DEFAULT(stat, _) -> self#stat2str stat
	| LABEL(_, stat, _) -> self#stat2str stat
	| COMPGOTO(exp,_) -> self#exp2str exp
	| DEFINITION d -> self#defs2str d
	| TRY_FINALLY(b, h, _) -> String.concat "\n" [ self#block2str b; self#block2str h]
	| TRY_EXCEPT(b, e, h, _) -> String.concat "\n" 
		[ self#block2str b; self#exp2str e; self#block2str h]
	| _ -> ""

	method block2str blk = 
		String.concat "\n" (List.map self#stat2str blk.bstmts)
	
	method defs2str = function 
	  FUNDEF(s, b, _, _) -> 
		(match ((fun (sl, n) -> self#name2declstr n) s) with
		Some x -> (inFunc <- false; 
				((self#singlen2str s); (self#block2str b) ;
				if inFunc = true then (self#singlen2str s) else ""))
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
(*	print_string "--->\n";
	print_string "<----\n" *)
	print_string (String.concat "\n" (List.map self#defs2str cab))
	end

end (* end class *)



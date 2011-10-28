open Unix 
open Cil
open Cabs
open Cprint

(* parse C source file and get function declaration *)
let print_string s = Pervasives.print_string s
  
class funlister = object (self)
    
  method type2str = function 
      Tvoid -> "void"
    | Tchar -> "char"
    | Tint -> "int"
    | Tshort -> "short"
    | Tlong -> "long"
    | Tint64 -> "int64"
    | Tfloat -> "float"
    | Tdouble -> "double"
    | Tsigned -> "signed"
    | Tunsigned -> "unsigned"
    | Tnamed s -> s 
    | _ -> ""
	  
  method spec2str = function 
      SpecTypedef -> "typedef"
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
      JUSTBASE -> if n <> "___missing_field_name" then
	n
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

  method exp2str = function NOTHING -> ""
    | UNARY (u, e) -> "UNARY"
    | LABELADDR(s) -> "LABELADDR"
    | BINARY(bo, e1, e2) -> "BINARY"
    | QUESTION(e1, e2, e3) -> "QUESTION"
    | CAST((s,d), ie) -> "CAST"
    | CALL(e, el) -> "CALL"
    | COMMA(el) -> "COMMA"
    | CONSTANT(c) -> "CONST"
    | PAREN(e) -> "PAREN"
    | VARIABLE(s) -> "VARIABLE"
    | EXPR_SIZEOF(e) -> "ESIZEOF"
    | TYPE_SIZEOF(s,d) -> "TSIZEOF"
    | EXPR_ALIGNOF(e) -> "EALIGNOF"
    | TYPE_ALIGNOF(s,d) -> "TALIGNOF"
    | INDEX(e1,e2) -> "INDEX"
    | MEMBEROF(e, s) -> "MEMBEROF"
    | MEMBEROFPTR(e, s) -> "MEMBEROFPTR"
    | GNU_BODY(b) -> "GNU_BODY"
    | EXPR_PATTERN(s) -> "EXPR_PATTERN"

  method const2str = function CONST_INT(s) -> s
    | CONST_FLOAT(s) -> s
    | CONST_CHAR(il) -> "CHAR"
    | CONST_WCHAR(il) -> "WCHAR"
    | CONST_STRING(s) -> s
    | CONST_WSTRING(il) -> "WSTRING"

  method name2str (n,decl,attrs, _) = 
    (self#decl2str n decl) ^ "" ^ self#attrs2str attrs

  method singlen2str ((spec:spec_elem list), (n:name)) = 
    let sl = List.map (fun x -> self#spec2str x) spec in
    if (List.exists (function "storage" -> true| _ -> false) sl) then
      if (List.exists (function "inline" -> true| _ -> false) sl) then
        ""
      else 
        (String.concat " " sl) ^ " "^ (self#name2str n)  
    (* "" if you want to display static, please change it *) 
    else (String.concat " " sl) ^ " "^ (self#name2str n)

  method defs2str = function 
      FUNDEF(s, b, _, _) -> self#singlen2str s
    | DECDEF(ig, _) -> ""
    | TYPEDEF(ng,_) -> ""
    | ONLYTYPEDEF(spec, _) -> ""
    | GLOBASM(s, _) -> ""
    | PRAGMA(e, _) -> ""
    | LINKAGE(s, _, dl) -> ""
    | TRANSFORMER(d, dl, _) -> ""
    | EXPRTRANSFORMER(e1, e2, _) -> ""

  method parse (cab:definition list) = 
    begin 
      print_string ((String.concat "\n" 
		      (List.filter (fun x -> (String.compare "" x)!=0) 
			 (List.map self#defs2str cab))) ^ "\n")
    end
end


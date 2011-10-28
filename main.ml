(*
 * cLister main routine
 *)
open Unix
open Cil
open Dotcallerprinter
open Callerprinter
open Calleeprinter
open Funlister
open Funnamelister

let print_string = Pervasives.print_string

(* Get arguments *)
let argget n = Array.get Sys.argv n
let arglen = Array.length Sys.argv

let _ =
  (* if cLister <filename>, then list function name in code. *)
  if arglen = 2 then
    let parser = new funlister 
    and ((_, deflist), _) = 
      Frontc.parse_with_cabs (Array.get Sys.argv 1) () 
    in
    (parser#parse deflist  ; ())
      
  else if arglen = 3 then
    match (Array.get Sys.argv 1) with 
      "funname" -> 
	let parser = new funnamelister 
	and ((_, deflist), _) = 
	  Frontc.parse_with_cabs (Array.get Sys.argv 2) () 
	in
	(parser#parse deflist ; ()) 

    | "dotcaller" -> 
	let parser = new dotCallerPrinterClass
	and ((_, deflist), _) = 
	  Frontc.parse_with_cabs (Array.get Sys.argv 2) () 
	in 
	(parser#parse deflist ; ()) 

    | _ -> print_string "???"
	  
  else if  arglen = 4 then
    let ((_, deflist), _) = 
      Frontc.parse_with_cabs (argget 2) () 
    in
    begin
      match (Array.get Sys.argv 1) with
	"caller" -> 
	  let parser = new callerPrinterClass (argget 3) 
	  in
	  (parser#parse deflist ; ())
      | "callee" ->
	  let parser = new calleePrinterClass (argget 3) 
	  in
	  parser#parse deflist  
	    | _ -> print_string "???"
    end
  else 
    begin
      print_string ("usage: " ^ (argget 0) ^ "\n");
      Pervasives.flush_all ()
    end
;;

fun isNull1 L = if L=[] then true else false;

fun isNull2 [] = true
	| isNull2 (x::rest) = false; (*this implementation is better because it does not enford the use of comparable types(x::rest) = pattern/one or more elements*)
	
fun f1 x = x+1;
	
fun X 0=0
	|	X 1=1
	|	X 2=2
	| 	X a=10;

(* RANDOM NOTES 
function types can be polytypes ( a' * a' = any but same type) (a' * b' = any including different types)

RECURSIVE FUNCTIONS USING PATTERNS:
*)

fun length [] = 0
	| length (_::rest) = 1 + (length rest); (* Calculate list of any list recursively*)
	
fun sumList [] = 0
	| sumList (x::rest) = x + (sumList rest);
	
fun append ([], L) = L
	| append(x::rest, L) = (x::append (rest, L));
	
fun reverse-append [] L = L
	| reverse-append(x::rest, L) = reverse-append rest (x::L);
	
	Standard ML of New Jersey v110.82 [built: Wed Oct 25 10:36:05 2017]
- val L = ["cool", "stupid", "dumb", "strings"];
val L = ["cool","stupid","dumb","strings"] : string list
- fun nthelement [] n = "N/A"
= | nthelement (x::rest) 1 = x
= | nthelement (x::rest) n = nthelement rest (n-1);
val nthelement = fn : string list -> int -> string
- nthelement L 2;
val it = "stupid" : string

(* Tuple stuff, and a bit o lists*)
Standard ML of New Jersey v110.82 [built: Wed Oct 25 10:36:05 2017]
- val t1 = ([1,2,3], [4,5]);
val t1 = ([1,2,3],[4,5]) : int list * int list
- #2 t1;
val it = [4,5] : int list
- (#2 t1) #2;
stdIn:3.1-3.11 Error: operator is not a function [tycon mismatch]
  operator: int list
  in expression:
    ((fn {2=<pat>,...} => 2) t1) (fn {2=2,...} => 2)
- val L1 (#2 t1);
stdIn:1.10-1.12 Error: syntax error: deleting  HASH INT
- val L1 = (#2 t1);
val L1 = [4,5] : int list
- L1[2]
= ;
stdIn:4.1-4.6 Error: operator is not a function [tycon mismatch]
  operator: int list
  in expression:
    L1 (2 :: nil)
- val ([x,y,z], L2) = t1;
stdIn:1.6-4.5 Warning: binding not exhaustive
          (x :: y :: z :: nil,L2) = ...
val x = 1 : int
val y = 2 : int
val z = 3 : int
val L2 = [4,5] : int list
-
(* Number 1 A) *)
- fun exists (x,[]) = false
= | exists (x,y::rest) = if y = x then true
= else exists (x, rest);
stdIn:42.29 Warning: calling polyEqual
val exists = fn : ''a * ''a list -> bool

(* Number 1 B)
The type is ''a * ''a List because the use of the comparison operator "=" enforces that the value type is comparable to the elements in the List*)

(* Number 1 C*)
- fun countInList [] x = 0
= | countInList (y::rest) x = if y = x then 1 + countInList rest x
= else countInList rest x;

(* Number 3*)
- fun firstN [] n = []
= | firstN L 0 = []
= | firstN (x::rest) n = x::firstN rest (n-1);
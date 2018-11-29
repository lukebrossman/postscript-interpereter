(* Number 1 A) *)
fun exists (x,[]) = false
| exists (x,y::rest) = if y = x then true
else exists (x, rest);

(* Number 1 B)
The type is ''a * ''a List because the use of the comparison operator "=" enforces that the value type is comparable to the elements in the List*)

(* Number 1 C*)
fun countInList [] x = 0
| countInList (y::rest) x = if y = x then 1 + countInList rest x
else countInList rest x;

(* Number 2 *)
fun listDiff ([],[]) = nil
| listDiff([], L) = nil
| listDiff(L,[]) = L
| listDiff(x::rest, L) = if exists (x,L) = false then x::listDiff(rest,L) else if countInList rest x >= countInList L x then x::listDiff(rest,L) else listDiff(rest,L);


(* Number 3*)
fun firstN [] n = []
| firstN L 0 = []
| firstN (x::rest) n = x::firstN rest (n-1);

(* Number 4*)
fun first  (x,y) = x;
fun second (y,x) = x;

fun busfinder x [] = []
| busfinder x (y::rest) = if exists (x, second y) = true then (first y)::busfinder x rest else busfinder x rest;

(* busFinder returns a 'b list because no comparison is done on the first element of the tuple ('b) that necessitates it be of the same generic type ans the other value, and list of values*) 


(* Number 5*)
fun sumOfInvertedResistors [] = 0.0
| sumOfInvertedResistors (x::rest) = (1.0/x) + sumOfInvertedResistors rest;
fun parallelResistors L = 1.0 / (sumOfInvertedResistors L);

(*this function cannot work when a resistor has a value of 0ohms or with an empty list of resistors because you cannot divide by zero*)

(* Number 6*)
fun countList [] = 0
| countList (x::rest) = 1 + countList rest;

fun lastN [] n = []
| lastN L 0 = L
| lastN (x::rest) n = lastN rest (n-1);

fun modList [] n = 0
| modList L n = if countList L < n then countList L else modList (lastN L n) n;

fun pairNright (n,[]) = nil
| pairNright (n,L) = (firstN L n)::(pairNright (n, lastN L n));

fun pairNleft (n,[]) = nil
| pairNleft (n,L) = if (modList L n > 0) then firstN L (modList L n)::pairNleft (n,lastN L n) else (firstN L n)::pairNleft (n, lastN L n);

fun pairNleft (n,[]) = nil
| pairNleft (n,L) = if (modList L n > 0) then firstN L (modList L n)::pairNleft (n,lastN L (modList L n)) else (firstN L n)::pairNleft (n, lastN L n);

(* tests*)
(* Exists Tests*)
fun existsTest () =
let

	val testList1 = [1,3,5]
	val testList2 = ["this", "is", "a", "test"]
	
	val existsT1 = (exists (1,testList1) = true)
	val existsT2 = (exists ("test",testList2) = true)
	val existsT3 = (exists (7,testList1) = false)
	val existsT4 = (exists (1,[]) = false)
	
in
	print ("exists:-------------------- \n"   ^ 
            "   test1: " ^ Bool.toString(existsT1) ^ "\n" ^ 
            "   test2: " ^ Bool.toString(existsT2) ^ "\n" ^  
			"   test3: " ^ Bool.toString(existsT3) ^ "\n" ^ 
            "   test4: " ^ Bool.toString(existsT4) ^ "\n")	
end;
val ex = existsTest ();

(* CountInList Tests*)
fun countInListTest () =
let

	val testList1 = [1,3,3,3,5]
	val testList2 = ["this", "is", "is", "a", "test"]
	
	val countInListT1 = (countInList testList1 3 = 3)
	val countInListT2 = (countInList testList2 "is" = 2)
	val countInListT3 = (countInList testList1 1 = 1)
	val countInListT4 = (countInList testList2 "nope" = 0)
	
in
	print ("countInList:-------------------- \n"   ^ 
            "   test1: " ^ Bool.toString(countInListT1) ^ "\n" ^ 
            "   test2: " ^ Bool.toString(countInListT2) ^ "\n" ^  
			"   test3: " ^ Bool.toString(countInListT3) ^ "\n" ^ 
            "   test4: " ^ Bool.toString(countInListT4) ^ "\n")	
end;
val cil = countInListTest ();

(*ListDiff Tests*)
fun listDiffTest () =
let

	val testList1 = [1,3,3,3,5]
	val testList2 = [3,5,4]
	val testList3 = ["this", "is", "a", "test"]
	val testList4 = ["this", "is", "a", "test", "list"]
	
	val listDiffT1 = (listDiff (["a","b","c"],["b"]) = ["a", "c"])
	val listDiffT2 = (listDiff (testList2,testList2) = [])
	val listDiffT3 = (listDiff (testList2,testList1) = [4])
	val listDiffT4 = (listDiff (testList3,testList4) = [])
	
in
	print ("listDiff:-------------------- \n"   ^ 
            "   test1: " ^ Bool.toString(listDiffT1) ^ "\n" ^ 
            "   test2: " ^ Bool.toString(listDiffT2) ^ "\n" ^  
			"   test3: " ^ Bool.toString(listDiffT3) ^ "\n" ^ 
            "   test4: " ^ Bool.toString(listDiffT4) ^ "\n")	
end;
val lDiff = listDiffTest ();

(* FirstN Tests*)
fun firstNTest () =
let

	val testList1 = [1,2,3,4,5]
	val testList2 = ["this", "is", "a", "test"]
	val testList3 = [[1,2],[3,4],[5,6]]
	
	val firstNT1 = ((firstN ["a", "b", "c", "x", "y"] 3) = ["a", "b", "c"])
	val firstNT2 = ((firstN testList1 0) = [])
	val firstNT3 = ((firstN testList2 1) = ["this"])
	val firstNT4 = ((firstN testList3 4) = [[1,2],[3,4],[5,6]])
	
in
	print ("firstN:-------------------- \n"   ^ 
            "   test1: " ^ Bool.toString(firstNT1) ^ "\n" ^ 
            "   test2: " ^ Bool.toString(firstNT2) ^ "\n" ^  
			"   test3: " ^ Bool.toString(firstNT3) ^ "\n" ^ 
            "   test4: " ^ Bool.toString(firstNT4) ^ "\n")	
end;
val firN = firstNTest ();

(* BusFinder Tests*)
fun busFinderTest () =
let  
	
  val buses = 
	[("Lentil",["Chinook", "Orchard", "Valley", "Emerald","Providence", "Stadium", "Main", "Arbor", "Sunnyside", "Fountain", "Crestview", "Wheatland", "Walmart", "Bishop", "Derby", "Dilke"]), 
	("Wheat",["Chinook", "Orchard", "Valley", "Maple","Aspen", "TerreView", "Clay", "Dismores", "Martin", "Bishop", "Walmart", "PorchLight", "Campus"]), 
	("Silver",["TransferStation", "PorchLight", "Stadium", "Bishop","Walmart", "Shopco", "RockeyWay"]),
	("Blue",["TransferStation", "State", "Larry", "TerreView","Grand", "TacoBell", "Chinook", "Library"]),
	("Gray",["TransferStation", "Wawawai", "Main", "Sunnyside","Crestview", "CityHall", "Stadium", "Colorado"])]
     
  val busFinderT1 = ((busfinder "Walmart" buses) = ["Lentil","Wheat","Silver"])
  val busFinderT2 = ((busfinder "Shopco" buses) = ["Silver"])
  val busFinderT3 = ((busfinder "Main" buses) = ["Lentil","Gray"])

 in 
     print ("busFinder:-------------------- \n"   ^ 
            "   test1: " ^ Bool.toString(busFinderT1) ^ "\n" ^ 
            "   test2: " ^ Bool.toString(busFinderT2) ^ "\n" ^  
            "   test3: " ^ Bool.toString(busFinderT3) ^ "\n")		
end;

val x = busFinderTest ();

(*ParallelResistors Tests *)
fun parallelResistorsTest () =
let
	
	val parallelResistorsT1 = Real.==(parallelResistors [10.0, 10.0, 10.0, 10.0], 2.5)
	val parallelResistorsT2 = Real.==(parallelResistors [8.0, 16.0, 4.0, 16.0], 2.0)
	val parallelResistorsT3 = Real.==(parallelResistors [5.0, 10.0, 2.0], 1.25)
	
in
	print ("exists:-------------------- \n"   ^ 
            "   test1: " ^ Bool.toString(parallelResistorsT1) ^ "\n" ^ 
            "   test2: " ^ Bool.toString(parallelResistorsT2) ^ "\n" ^  
			"   test3: " ^ Bool.toString(parallelResistorsT3) ^ "\n")	
end;
val pR = parallelResistorsTest ();

(*PairNRight Tests*)
fun pairNrightTest () =
let

	
	val pairNrightT1 = (pairNleft (2,[1, 2, 3, 4, 5]) = [[1, 2], [3, 4], [5]])
	val pairNrightT2 = (pairNleft (3,[1, 2, 3, 4, 5]) = [[1, 2, 3], [4, 5]])
	val pairNrightT3 = (pairNleft (1,[1, 2, 3, 4, 5]) = [[1], [2], [3], [4], [5]])
	val pairNrightT4 = (pairNleft (4,[1, 2, 3, 4, 5]) = [[1, 2, 3, 4], [5]])
	
in
	print ("pairNright:-------------------- \n"   ^ 
            "   test1: " ^ Bool.toString(pairNrightT1) ^ "\n" ^ 
            "   test2: " ^ Bool.toString(pairNrightT2) ^ "\n" ^  
			"   test3: " ^ Bool.toString(pairNrightT3) ^ "\n" ^ 
            "   test4: " ^ Bool.toString(pairNrightT4) ^ "\n")	
end;
val pNr = pairNrightTest ();

(* PairNLeft Tests*)
fun pairNleftTest () =
let

	
	val pairNleftT1 = (pairNleft (2,[1, 2, 3, 4, 5]) = [[1], [2,3], [4,5]])
	val pairNleftT2 = (pairNleft (3,[1, 2, 3, 4, 5]) = [[1, 2, 3], [4, 5]])
	val pairNleftT3 = (pairNleft (1,[1, 2, 3, 4, 5]) = [[1], [2], [3], [4], [5]])
	val pairNleftT4 = (pairNleft (4,[1, 2, 3, 4, 5]) = [[1], [2, 3, 4,5]])
	
in
	print ("pairNleft:-------------------- \n"   ^ 
            "   test1: " ^ Bool.toString(pairNleftT1) ^ "\n" ^ 
            "   test2: " ^ Bool.toString(pairNleftT2) ^ "\n" ^  
			"   test3: " ^ Bool.toString(pairNleftT3) ^ "\n" ^ 
            "   test4: " ^ Bool.toString(pairNleftT4) ^ "\n")	
end;
val pNl = pairNleftTest ();





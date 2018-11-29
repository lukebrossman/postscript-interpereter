(* Number 1 A.*)
fun numbersToSum sum [] = []
| numbersToSum sum (x::rest) = if sum - x >= 0 then x::numbersToSum (sum - x) rest else [];

(* Number 1 B.*)


(* Number 2 *)
fun filterTrue pred [] = []
| filterTrue pred (x::rest) = if pred x then x::(filterTrue pred rest)
else (filterTrue pred rest);

fun filterFalse pred [] = []
| filterFalse pred (x::rest) = if (pred x) = false then x::(filterFalse pred rest)
else (filterFalse pred rest);

fun partition f L = (filterTrue f L, filterFalse f L);

(* Number 3 *)

fun exists x [] = false
| exists x (y::rest) = if y = x then true
else exists x rest;

fun filterUnique pred [] = false
| filterUnique pred (x::rest) = if pred x rest then true else (filterUnique pred rest);

fun areAllUnique [] = true
| areAllUnique L = if filterUnique exists L then false else true;

(* Number 4? A*)
fun map f [] = []
| map f (x::rest) = (f x)::map f rest;

fun add x y = x + y;

fun fold f base [] = base
| fold f base (x::rest) = f x (fold f base rest);

fun foldAdd L = (fold add 0 L);

fun sum [] = 0
| sum L = foldAdd(map foldAdd L);

(* Number 4 B*)
fun addOption (x:int option) NONE = x
| addOption (x:int option) (y:int option) = SOME(valOf(x) + valOf(y));

fun foldAddOption L = (fold addOption NONE L);

fun sumOption [] = NONE
| sumOption L = foldAddOption(map foldAddOption L);

(*Number 4 C*)

datatype either = IString of string | IInt of int;

fun str2int s = valOf(Int.fromString(s));

fun addEither (IString x) (IString y) = IInt(str2int x + str2int y)
| addEither (IString x) (IInt y) = IInt(str2int x + y)
| addEither (IInt x) (IString y) = IInt(x + str2int y)
| addEither (IInt x) (IInt y) = IInt(x + y);

fun foldAddEither L = (fold addEither (IInt 0) L);

fun sumEither [] = (IInt 0)
| sumEither L = foldAddEither(map foldAddEither L);

(*Number 5 A.*)
datatype 'a Tree = LEAF of 'a | NODE of 'a * ('a Tree) * ('a Tree)

fun left (x,y,z) = y;
fun right (x,y,z) = z;
fun value (x,y,z) = [x];

fun depthScan (LEAF x) = [x]
| depthScan (NODE x) = (depthScan (left x))@((depthScan (right x))@(value x));

(* Number 5 B.*)
 fun valueCompare x y = if (x = y) then 1 else ~1;

fun depthSearch(LEAF (x)) n = if (x = n) then 1 else ~1 
| depthSearch(NODE (x, y, z)) n =
if (valueCompare x n) =1 then 1
 else if(depthSearch y n) <> ~1 then (depthSearch y n)+1
 else if (depthSearch z n) <> ~1 then (depthSearch z n) +1
 else ~1;


(*Number 5 C.*)
fun addTrees (LEAF x) (LEAF y) = LEAF(x+y)
| addTrees (LEAF x) (NODE (y,z,w)) = NODE(x+y,z,w)
| addTrees (NODE (x,y,z)) (LEAF w) = NODE (x+w,y,z)
| addTrees (NODE (x1,y1,z1)) (NODE (x2,y2,z2)) = NODE(x1+x2, addTrees y1 y2, addTrees z1 z2);

fun allSumsTest()=
let
		val sumT1 = sum [[1,2,3],[4,5],[6,7,8,9],[]] = 45
		val sumT2 = sum [[10,10],[10,10,10],[10]] = 60
		val sumT3 = sum [[]] = 0
		val sumOptionT1 = sumOption [[SOME(1),SOME(2),SOME(3)],[SOME(4),SOME(5)],[SOME(6),NONE],[],[NONE]] = SOME 21
		val sumOptionT2 = sumOption [[SOME(10),NONE],[SOME(10), SOME(10), SOME(10),NONE,NONE]] = SOME 40
		val sumOptionT3 = sumOption [[NONE]] = NONE
		val sumEitherT1 = sumEither [[IString "1",IInt 2,IInt 3],[IString "4",IInt 5],[IInt 6,IString"7"],[],[IString "8"]] = IInt 36
		val sumEitherT2 = sumEither [[IString "10" , IInt 10],[],[IString "10"],[]] = IInt 30
		val sumEitherT3 = sumEither [[]] = IInt 0
	in
		print(
            "   sum 1: " ^ Bool.toString(sumT1) ^ "\n" ^ 
            "   sum 2: " ^ Bool.toString(sumT2) ^ "\n" ^  
			"   sum 3: " ^ Bool.toString(sumT3) ^ "\n" ^  
			"   sumOption 1: " ^ Bool.toString(sumOptionT1) ^ "\n" ^
			"   sumOption 2: " ^ Bool.toString(sumOptionT2) ^ "\n" ^
			"   sumOption 3: " ^ Bool.toString(sumOptionT3) ^ "\n" ^
			"   sumEither 1: " ^ Bool.toString(sumEitherT1) ^ "\n" ^
			"   sumEither 2: " ^ Bool.toString(sumEitherT2) ^ "\n" ^
			"   sumEither 3: " ^ Bool.toString(sumEitherT3) ^ "\n"
			)
	end;
	

val allSums = allSumsTest();

fun partitionTest ()= 
	let
		val partitionT1 = partition (fn x => (x<=4)) [1,7,4,5,3,8,2,3] = ([1,4,3,2,3],[7,5,8])
		val partitionT2 = partition null [[1,2],[1],[],[5],[],[6,7,8]] = ([[],[]],[[1,2],[1],[5],[6,7,8]])
		fun exists n [] = false
			| exists n (x::rest) = if n=x then true else (exists n rest)
		val partitionT3 = partition (exists 1) [[1,2],[1],[],[5],[],[6,7,8]] = ([[1,2],[1]],[[],[5],[],[6,7,8]])
		
	in
		print(
            "   partition 1: " ^ Bool.toString(partitionT1) ^ "\n" ^ 
            "   partition 2: " ^ Bool.toString(partitionT2) ^ "\n" ^  
			"   partition 3: " ^ Bool.toString(partitionT3) ^ "\n"
			)
			
	end;

val part = partitionTest();

fun areAllUniqueTest ()= 
	let
		val areAllUniqueT1 = areAllUnique [1,3,4,2,5,0,10] = true
		val areAllUniqueT2 = areAllUnique [[1,2],[3],[4,5],[]] = true
		val areAllUniqueT3 = areAllUnique [(1,"one"),(2,"two"),(1,"one")] = false
		val areAllUniqueT4 = areAllUnique [] = true
		
	in
		print(
            "   areAllUnique 1: " ^ Bool.toString(areAllUniqueT1) ^ "\n" ^ 
            "   areAllUnique 2: " ^ Bool.toString(areAllUniqueT2) ^ "\n" ^  
			"   areAllUnique 3: " ^ Bool.toString(areAllUniqueT3) ^ "\n"  
			)
			
	end;

val areunique = areAllUniqueTest();


fun depthScanTest ()= 
	let
		val depthScanT1 = depthScan (NODE("Science",NODE ("and",LEAF "School", NODE("Engineering", LEAF "of",LEAF "Electrical")),LEAF "Computer")) = ["School","of","Electrical","Engineering","and","Computer","Science"]
		val depthScanT2 = depthScan (NODE(1, NODE (2, NODE(3, LEAF 4 ,LEAF 5),LEAF 6), NODE(7,LEAF 8,LEAF 9))) = [4,5,3,6,2,8,9,7,1]
		val depthScanT3 = depthScan (LEAF 4) = [4]
		
	in
		print(
            "   depthScan 1: " ^ Bool.toString(depthScanT1) ^ "\n" ^ 
            "   depthScan 2: " ^ Bool.toString(depthScanT2) ^ "\n" ^  
			"   depthScan 3: " ^ Bool.toString(depthScanT3) ^ "\n"  
			)
			
	end

val dscan = depthScanTest();


fun depthSearchTest ()= 
	let
		val myT = NODE(1, NODE (2, NODE(3, LEAF 2 ,LEAF 5),LEAF 1), NODE(1,LEAF 8,LEAF 5))

		val depthSearchT1 = depthSearch myT 1 = 3 (* This is definitely not correct, depthsearch of 1 should = 1. will return false for test case ????*)
		val depthSearchT2 = depthSearch myT 5 = 4
		val depthSearchT3 = depthSearch myT 8 = 3
	in
		print(
            "   depthSearch 1: " ^ Bool.toString(depthSearchT1) ^ "\n" ^ 
            "   depthSearch 2: " ^ Bool.toString(depthSearchT2) ^ "\n" ^  
			"   depthSearch 3: " ^ Bool.toString(depthSearchT3) ^ "\n"  
			)	
	end

val dsearch = depthSearchTest();


(*fun Q5test ()=*) 




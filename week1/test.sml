
val test01 = alternate([1,2,3,4])= (~2);
val test02 = alternate([4,1,1,1])  =3;
val test03 = alternate([1,1,1,1])  =0;
val test04 = alternate([10,9,2,2]) =1;

val test11 = min_max([1,2,3,4])=(1,4);
val test12 = min_max([4,1,2,3])=(1,4);
val test13 = min_max([~1,~2,~3,~4])=(~4,~1);

val test21 = cumsum([1,4,20])=[1,5,25];
val test21 = cumsum([1,1,1])=[1,2,3];

val test31 = greeting(SOME("Priyank")) = "Hello there, Priyank!";
val test32 = greeting(NONE) = "Hello there, you!";

val test41 = repeat([1,2,3],[4,0,3])=[1,1,1,1,3,3,3];
val test42 = repeat([1,2,3],[2,1,3])=[1,1,2,3,3,3];
val test43 = repeat([1,2],[2,1,3])=[1,1,2];
val test44 = repeat([1,2,3],[2,1])=[1,1,2];
val test51 = addOpt(SOME(1),SOME(2))=SOME(3) 
val test52 = addOpt(SOME(1),NONE)=NONE 
val test53 = addOpt(NONE, SOME(1))=NONE 
val test54 = addOpt(NONE, NONE)=NONE 


val test61 = addAllOpt([SOME(1), SOME(2), NONE, SOME(3)]) = SOME(6) 
val test62 = addAllOpt([NONE, NONE, NONE]) = NONE
val test63 = addAllOpt([NONE, NONE, SOME(1)]) = SOME(1)
val test64 = addAllOpt([NONE, SOME(1), NONE]) = SOME(1)

val test71 = any([true,true,true])=true
val test72 = any([true,true,false])=true
val test73 = any([false,true,false])=true
val test74 = any([false,false,false])=false

val test81 = all([true,true,true])=true
val test82 = all([true,true,false])=false
val test83 = all([false,true,false])=false
val test84 = all([false,false,false])=false

val test90 = zip([1,2,3],[4,5,6]) = [(1,4),(2,5),(3,6)]
val test91 = zip([1,2,3],[4,5]) = [(1,4),(2,5)]
val test92 = zip([1,2],[4,5,6]) = [(1,4),(2,5)]

val test100 = lookup([("a",1),("b",2),("c",3)],"a") = SOME(1)
val test101 = lookup([("a",1),("b",2),("c",3)],"b") = SOME(2)
val test102 = lookup([("a",1),("b",2),("c",3)],"c") = SOME(3)
val test103 = lookup([("a",1),("b",2),("c",3)],"d") = NONE

val test110 = splitup([1,2,3,4]) = ([1,2,3,4],[])
val test111 = splitup([~1,~2,~3,~4]) = ([],[~1,~2,~3,~4])
val test112 = splitup([1,~2,3,~4]) = ([1,3],[~2,~4])

val test120 = splitAt([1,2,5,6],3) = ([5,6],[1,2])
val test121 = splitAt([~1,~2,~5,~6],~4) = ([~1,~2],[~5,~6])
val test122 = splitAt([1,~2,3,~4],0) = ([1,3],[~2,~4])

val test130 = isSorted([1,2,3,4]) = true
val test131 = isSorted([1,2,4,3]) = false
val test132 = isSorted([1,1,1,1]) = false
val test133 = isSorted([2,1,3,4]) = false
val test134 = isSorted([]) = true
val test135 = isSorted([2]) = true

val test140 = isAnySorted([1,2,3,4]) = true
val test141 = isAnySorted([4,3,2,1]) = true
val test142 = isAnySorted([1,2,4,3]) = false
val test143 = isAnySorted([]) = true
val test144 = isAnySorted([2]) = true

val test150 = sortedMerge([1,3,5],[2,4,6]) = [1,2,3,4,5,6]
val test151 = sortedMerge([1,3,5],[]) = [1,3,5]
val test152 = sortedMerge([],[2,4,6]) = [2,4,6]
val test153 = sortedMerge([],[]) = []
val test154 = sortedMerge([1,3,7],[10,12,16]) = [1,3,7,10,12,16]

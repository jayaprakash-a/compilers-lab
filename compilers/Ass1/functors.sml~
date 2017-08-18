signature ORD =
sig

type t

val le : t -> t -> bool

end

structure IntOrd : ORD = 
struct

type t = int;

fun le a b = if (a <= b) then true else false 
end

functor QSort (X : ORD ) = struct


(*x = [1,2,3,4];*)
(*

val first = List.nth(x, 0)

val (left, right) = List.partition(fn x => x <= first);*)

(*fun qsort arr = if (List.length(arr) == 1) then arr
					else (qsort(left)@qsort(right))   *)

fun qsort [] = [] 
 | qsort (first :: xs) = let 
				val (left, right) = List.partition(X.le first) xs
			   in 
					qsort(left)@[first]@qsort(right) 
			   end

end

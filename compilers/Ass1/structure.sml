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

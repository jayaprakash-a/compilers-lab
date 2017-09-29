(* signature Graph = sig
   eqtype node
   type   graph     (* mutable variant of a graph *)

   val newNode : graph       -> node
   val addEdge : (node * node) -> graph (*-> ()*)
   val nodes   : graph       -> node list
   val suc     : graph       -> node list
   val pred    : graph       -> node list

end


structure mygraph : Graph = 
struct

end*)



datatype vertex = Node of (int * int);

datatype adjacent = Adj of vertex list;
datatype adjacencylist = List of (vertex * adjacent);
datatype graph = Graph of (adjacencylist list);

fun getlist( Graph(adj_list)) = adj_list;

fun getadjelements( List(node,adjelement) )= adjelement;
fun getnode( List(node,adjelement)) = node;

fun getverno(Node(x,y)) = x;

fun nodeispresent [] Node(y1,y2) = false |
nodeispresent (x :xs) Node(y1,y2)  =  getverno (getnode x) = y1 orelse nodeispresent xs Node(y1,y2);

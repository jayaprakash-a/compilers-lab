signature base_node = sig
  eqtype node
  val compare : node * node -> order
end

(*Basic functor for the graph*)
functor Initilise_graph(Graph: base_node) =
struct
structure MapT = RedBlackMapFn (
				struct type ord_key = Graph.node
					   val compare = Graph.compare
				end)
val mygraph = MapT.empty

(*Function to add edges*)
fun add_edge(mygraph,node,adj_ver)  = 
			if MapT.find(mygraph,node) = NONE then MapT.insert(mygraph,node,[adj_ver])
                                    else
                                      let val SOME(x) = MapT.find(mygraph,node) in
                                          MapT.insert(mygraph,node,adj_ver::x)
                                   
                                      end
				


(*Returns the list of nodes that have an edge from given node*)
fun successor(mygraph,node) = if MapT.find(mygraph,node) = NONE then []
                              else
	                            let val SOME(x) = MapT.find(mygraph,node) in 
	                            	x 
	                            end;

fun list_nodes(mygraph) = MapT.listKeys(mygraph)
(*To check if the node is adjacent to node. This is calculated by checking if given node is presnt in adjacency list of node*)
fun is_given_node_adjacent([], node) = false
	| is_given_node_adjacent(adj_list, node) = if List.find (fn y => (y = node)) adj_list = NONE then false
											 else true

(*Returns the list of nodes that have an edge to given node*)
fun predecessor(mygraph, [], node, pred) = pred
    | predecessor(mygraph, key::key_list, node, pred) =  let val SOME(adj_list) = MapT.find(mygraph,key)
                                                val new_pred =  if is_given_node_adjacent(adj_list, node) 
                                                then key::pred else pred in
                                                predecessor(mygraph, key_list, node, new_pred)
                                              end
end

structure vertex_base_type =
struct
  datatype node = Node of int
  val compare = fn (Node(x),Node(y)) => Int.compare(x,y)
end


structure Graph = Initilise_graph(vertex_base_type)
(*Returns true if node has only one outgoing edge*)
fun valid_no_of_links(mylist) = (List.length mylist = 1)

fun removeduplicate [] = []
| removeduplicate [x] = [x]
| removeduplicate (x::xs) =  if List.find (fn y => (y = x)) xs = NONE then
                              [x] @ removeduplicate(xs)
                          else removeduplicate(xs)

fun calculate_predecessor_chain(mygraph,node) = let val pred_list = Graph.predecessor(mygraph, Graph.list_nodes(mygraph), node, []) 
												    val suc_node_list = Graph.successor(mygraph, node) in

                                  if (valid_no_of_links(pred_list)) then 
								  	  let val suc_list =  Graph.successor(mygraph,hd pred_list) in
                                          if (valid_no_of_links(suc_list)) then 
											let val x = hd pred_list in
                                              (calculate_predecessor_chain(mygraph,x))@[node]
                                            end
                                          else

                                              [node]
                                      end
                                  else
                                      [node]
                                  end

fun calculate_successor_chain(mygraph, node) = let val suc_list = Graph.successor(mygraph, node) 
												   val pred_node_list = Graph.predecessor(mygraph, Graph.list_nodes(mygraph), node, [])in
	                                if (valid_no_of_links(suc_list)) then 
										let val pred_list =  Graph.predecessor(mygraph, Graph.list_nodes(mygraph), hd suc_list, []) in
	                                      if (valid_no_of_links(pred_list)) then 
	                                      	let val x = hd suc_list in
	                                          [node]@(calculate_successor_chain(mygraph, x))
	                                        end
	                                      else
	                                        [node]
	                                    end
	                                else
	                                    [node]
	                            
 								end

fun list_basic_block_node(mygraph, node) = let val p_chain = calculate_predecessor_chain(mygraph, node)in

                                        	let val s_chain = calculate_successor_chain(mygraph, node) in
                                        	 
	                                         	p_chain@(tl s_chain)
	                                        end
											end

fun list_basic_block_all_nodes(mygraph, []) = []
	| list_basic_block_all_nodes(mygraph, node::nodes) = list_basic_block_node(mygraph, node)::(list_basic_block_all_nodes(mygraph, nodes)) 


fun equal (a: int list, b: int list) = (a = b)
fun isInList ([], z) = false
  | isInList (x::xs, z) = x = z orelse isInList (xs, z)

structure AtomRedBlackMap =
	RedBlackMapFn (
		struct  type ord_key = Node of int
  val compare = fn (Node(x),Node(y)) => Int.compare(x,y)
end)


val in_map = AtomRedBlackMap.empty

val out_map = AtomRedBlackMap.empty              
val use_map = AtomRedBlackMap.empty              
val def_map = AtomRedBlackMap.empty              


val ex_graph = Graph.mygraph


val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 1),(vertex_base_type.Node 2));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 2),(vertex_base_type.Node 3));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 2),(vertex_base_type.Node 4));



fun add_use(node, use_map, use_nodes ) = if AtomRedBlackMap.find(use_map, node) = NONE then 
							AtomRedBlackMap.insert(use_map,node,use_nodes) 
else use_map

fun add_def(node, def_map, def_nodes ) = if AtomRedBlackMap.find(def_map, node) = NONE then 
							AtomRedBlackMap.insert(def_map,node,def_nodes) 
else def_map




val answer = list_basic_block_all_nodes(ex_graph, Graph.list_nodes(ex_graph));
val answer = removeduplicate(answer);


fun init_use_list [] = AtomRedBlackMap.empty
| init_use_list(x::xs) = let map_new = add_use(x, use_map, NONE) in add_use(hd xs,map_new, NONE) end


fun init_def_list [] = AtomRedBlackMap.empty
| init_def_list(x::xs) = let map_new = add_defx, def_map, NONE) in add_use(hd xs,map_new, NONE) end


val use_map = init_use_list(Graph.list_nodes(ex_graph))
val def_map = init_def_list(Graph.list_nodes(ex_graph))



val use_map = add_use(vertex_base_type.Node 1, use_map, ["b", "c"]);
val use_map = add_use(vertex_base_type.Node 2, use_map, ["a", "c"]);
val use_map = add_use(vertex_base_type.Node 3, use_map, ["b"]);


val def_map = add_def(vertex_base_type.Node 1, use_map, ["a"]);
val def_map = add_def(vertex_base_type.Node 2, use_map, ["b"]);
val def_map = add_def(vertex_base_type.Node 3, use_map, ["c"]);



fun len [] = 0
| len(x::xs) = 1 + len(xs)

val noofnodes = len(Graph.list_nodes(ex_graph))

fun hdlist [] = []
	| hdlist(x::xs) = [x]
fun int_node_list [] = []
	| int_node_list( (Node(x))::xs) = [x] @ int_node_list(xs) 
fun int_val(Node(x)) = x

signature Instruction =
sig
    type inst
    val useSet : inst -> AtomSet.set (* Get the use set of an instruction *)
    val defSet : inst -> AtomSet.set (* Get the def set of an instruction *)
end


fun inlist(y, x::xs) = if x = y then true else findlist(y, xs)
		| findlist(y, []) = false

fun union(x, y::ys) = if inlist(y, x)=true then union(x, ys) else union(y::x, ys)
		| union(x, []) = x

fun diff(x::xs, y) = if inlist(x, y)=true then diff(xs, y) else diff(xs, x::y)
		| diff([], y) = y

fun in_map_list(x, use_map,def_map,out_map) = let SOME(use_n) = AtomRedBlackMap.find(use_map, x) in 
											let SOME(out_n) = AtomRedBlackMap.find(out_map, x) in 
												let SOME(def_n) = AtomRedBlackMap.find(def_map,x) in

													union(use_n,dif(out_n,def_n))
												end
												end
											end
fun out_map_list(_,_,[]) = []

| out_map_list(x, in_map, suc::suc_list) = let SOME(in_n) = AtomRedBlackMap.find(in_map, suc) in 
												union(use_n,out_map_list(x,in_map,suc_list))

											end
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
fun add_edge(mygraph,node,adj_ver)  = if MapT.find(mygraph,node) = NONE then MapT.insert(mygraph,node,[adj_ver])
                                    else
                                      let val SOME(x) = MapT.find(mygraph,node) in
                                          MapT.insert(mygraph,node,adj_ver::x)
                                          (*if MapT.find(mygraph,adj_ver) = NONE then
                                          	MapT.insert(mygraph,adj_ver,[]:Graph.node list) 
                                      	  else 
                                      		let val SOME(y) = MapT.find(mygraph,adj_ver) in
                                      			MapT.insert(mygraph,adj_ver,y)
                                      		end*)
                                      end;


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
 (*                                 
fun exceptlast [] = []
	| exceptlast [x] = []
	| exceptlast (x::xs) = x::exceptlast(xs)

*)


fun list_basic_block_node(mygraph, node) = let val p_chain = removeduplicate(calculate_predecessor_chain(mygraph, node))in

                                        	let val s_chain = removeduplicate(calculate_successor_chain(mygraph, node)) in
                                        	 
	                                         	p_chain@(tl s_chain)
	                                        end
					end
(*
fun list_basic_block_node(mygraph, node) = let val p_chain = calculate_predecessor_chain(mygraph, node)
                                        	val s_chain = calculate_successor_chain(mygraph, node) in
                                        	 if(valid_no_of_links(calculate_predecessor_chain(mygraph, hd p_chain)) andalso valid_no_of_links(calculate_successor_chain(mygraph, hd p_chain)) ) then
                                        		if(s_chain = []) then 
                                        			(exceptlast(tl p_chain) )@[node]
                                        		else
	                                            	(exceptlast(tl p_chain) )@[node]@(tl s_chain)
	                                         else 
	                                         	if(s_chain = []) then 
                                        			(exceptlast(p_chain) )@[node]
                                        		else
	                                            	(exceptlast(p_chain) )@[node]@(tl s_chain)
	                                        end


structure AtomRedBlackMap =
	RedBlackMapFn (
		struct
			type ord_key = int
			val compare = fn (x,y) => Int.compare(x,y)
		end)

val block_map = AtomRedBlackMap.empty

fun getint(vertex_base_type.Node(x)) = x

fun list_basic_block_all_singular(block_map, mygraph, []) = block_map
	| list_basic_block_all_singular(block_map, mygraph, node::nodes) = let val p_chain = Graph.predecessor(mygraph, Graph.list_nodes(mygraph), node, [])
                                        								val s_chain = Graph.successor(mygraph, node)  in
		                                        			if(valid_no_of_links(p_chain) andalso valid_no_of_links(s_chain)) then 
		                                        				list_basic_block_all_singular(block_map, mygraph,nodes)

		                                        			else 
		                                        				if(AtomRedBlackMap.find(block_map, getint(node))) = NONE then
		                                        					AtomRedBlackMap.insert(block_map,getint(node),true)
		                                        				else block_map
                                        				end


fun check_fixed_point(block_map, [], mygraph) = block_map
	| check_fixed_point(block_map, node::nodes, mygraph) =

			let val block_map_new = list_basic_block_all_singular(block_map, mygraph, node::nodes) in

			if ( AtomRedBlackMap.listItems(block_map) = AtomRedBlackMap.listItems(block_map_new) )
			then block_map_new
			else
				check_fixed_point(block_map_new, nodes, mygraph)
			end;
*)
fun list_basic_block_all_nodes(mygraph, []) = []
	| list_basic_block_all_nodes(mygraph, node::nodes) = removeduplicate(list_basic_block_node(mygraph, node))::(list_basic_block_all_nodes(mygraph, nodes)) 


               
(*
val ex_graph = Graph.mygraph  

val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 1),(vertex_base_type.Node 2));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 2),(vertex_base_type.Node 3));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 3),(vertex_base_type.Node 1));
*)
(*
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 1),(vertex_base_type.Node 5));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 5),(vertex_base_type.Node 6));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 6),(vertex_base_type.Node 7));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 7),(vertex_base_type.Node 8));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 4),(vertex_base_type.Node 5));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 4),(vertex_base_type.Node 1));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 8),(vertex_base_type.Node 4));

val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 3),(vertex_base_type.Node 5));
*)


val ex_graph = Graph.mygraph

val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 4),(vertex_base_type.Node 2));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 2),(vertex_base_type.Node 3));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 3),(vertex_base_type.Node 1));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 1),(vertex_base_type.Node 5));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 5),(vertex_base_type.Node 6));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 6),(vertex_base_type.Node 7));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 7),(vertex_base_type.Node 8));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 4),(vertex_base_type.Node 5));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 4),(vertex_base_type.Node 1));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 8),(vertex_base_type.Node 4));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 3),(vertex_base_type.Node 5));

val answer = list_basic_block_all_nodes(ex_graph, Graph.list_nodes(ex_graph));
val answer = removeduplicate(answer);

(*
val mymap = check_fixed_point(block_map, Graph.list_nodes(ex_graph), ex_graph)*)
(*AtomRedBlackMap.listKeys(mymap);*)

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
val suc_graph = MapT.empty

val pred_graph = MapT.empty
(*Function to add edges*)
fun add_edge_succ(suc_graph,node,adj_ver)  = 
			if MapT.find(suc_graph,node) = NONE then MapT.insert(suc_graph,node,[adj_ver])
                                    else
                                      let val SOME(x) = MapT.find(suc_graph,node) in
                                          MapT.insert(suc_graph,node,adj_ver::x)
                                   
                                      end
				
fun add_edge_pred(pred_graph,node,adj_ver)  = if MapT.find(pred_graph,node) = NONE then MapT.insert(pred_graph,node,[adj_ver])
                                    else
                                      let val SOME(x) = MapT.find(pred_graph,node) in
                                          MapT.insert(pred_graph,node,adj_ver::x)
                                   
                                      end;

(*Returns the list of nodes that have an edge from given node*)
fun successor(suc_graph,node) = if MapT.find(suc_graph,node) = NONE then []
                              else
	                            let val SOME(x) = MapT.find(suc_graph,node) in 
	                            	x 
	                            end;

fun list_nodes(suc_graph) = MapT.listKeys(suc_graph)


(*Returns the list of nodes that have an edge to given node*)
fun predecessor(pred_graph,node) = if MapT.find(pred_graph,node) = NONE then []
                              else
	                            let val SOME(x) = MapT.find(pred_graph,node) in 
	                            	x 
	                            end;
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

fun calculate_predecessor_chain(mygraph,node, prev_list) = let val pred_list = Graph.predecessor(mygraph, Graph.list_nodes(mygraph), node, []) 
						val suc_node_list = Graph.successor(mygraph, node) in

                                  if (valid_no_of_links(pred_list)) then 
								  	  let val suc_list =  Graph.successor(mygraph,hd pred_list) in
                                          if (valid_no_of_links(suc_list)) then 
											let val x = hd pred_list in
											if List.find (fn y => (y = node)) (prev_list) <> NONE then 
												prev_list
											else 
                                             (calculate_predecessor_chain(mygraph,x, prev_list@[node]) )@[node]
												
                                            end
                                          else

                                              [node]
                                      end
                                  else
                                      [node]
                                  end

fun calculate_successor_chain(mygraph, node, prev_list) = let val suc_list = Graph.successor(mygraph, node) 
						 val pred_node_list = Graph.predecessor(mygraph, Graph.list_nodes(mygraph), node, [])in
                                
	                                if (valid_no_of_links(suc_list)) then 
						let val pred_list =  Graph.predecessor(mygraph, Graph.list_nodes(mygraph), hd suc_list, []) in
	                                      if (valid_no_of_links(pred_list)) then 
	                                      	let val x = hd suc_list in
	                                          (*[node]@(calculate_successor_chain(mygraph, x,))*)
												if List.find (fn y => (y = node)) (prev_list) <> NONE then 
												prev_list
											else 
                                             [node]@(calculate_predecessor_chain(mygraph,x, [node]@prev_list) )
												
	                                        end
	                                      else
	                                        [node]
	                                    end
	                                else
	                                    [node]
	                            
                               end
 

fun list_basic_block_node(mygraph, node) = let val p_chain = removeduplicate(calculate_predecessor_chain(mygraph, node,[]))in

                                        	let val s_chain = removeduplicate(calculate_successor_chain(mygraph, node,[])) in
                                        	 
	                                         	p_chain@(tl s_chain)
	                                        end
											end

fun list_basic_block_all_nodes(mygraph, []) = []
	| list_basic_block_all_nodes(mygraph, node::nodes) = removeduplicate(list_basic_block_node(mygraph, node))::(list_basic_block_all_nodes(mygraph, nodes)) 


structure AtomRedBlackMap =
	RedBlackMapFn (
		struct
			type ord_key = int
			val compare = fn (x,y) => Int.compare(x,y)
		end)

val in_map = AtomRedBlackMap.empty

structure AtomRedBlackMap =
	RedBlackMapFn (
		struct
			type ord_key = int
			val compare = fn (x,y) => Int.compare(x,y)
		end)

val out_map = AtomRedBlackMap.empty              

val ex_graph = Graph.mygraph  

val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 1),(vertex_base_type.Node 2));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 2),(vertex_base_type.Node 3));
val ex_graph = Graph.add_edge(ex_graph,(vertex_base_type.Node 3),(vertex_base_type.Node 1));

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

(*
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
*)
val answer = list_basic_block_all_nodes(ex_graph, Graph.list_nodes(ex_graph));
val answer = removeduplicate(answer);



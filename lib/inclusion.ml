module G = Graph.Pack.Digraph
module NodeMap = Map.Make (G.V)
module EdgeMap = Map.Make (G.E)

type inc = {nodes: G.t; edges: G.t}

type t = {id: int; src: int; dst: int; lhs: inc; rhs: inc}

let cpt = ref 0

let inc () =
  cpt := !cpt + 1 ;
  !cpt

let empty_inc () = {nodes= G.create (); edges= G.create ()}

let create vl vr el er sub over =
  Format.printf "Inclusion sub: %d over:%d len1 :%d len2 :%d len3 :%d len4 :%d \n" sub over (List.length vl)
  (List.length vr) (List.length el) (List.length er);
  let nodesl = G.create () in
  let edgesl = G.create () in
  let el = List.map (fun e -> G.V.create (G.E.label e)) (el@er) in
  List.iter (fun v -> G.add_vertex nodesl v) (vl@vr);
  List.iter (fun e -> G.add_vertex edgesl e) el ;

  let nodesr = G.create () in
  let edgesr = G.create () in
  (*let er = List.map (fun e -> G.V.create (G.E.label e)) er in
  List.iter (fun v -> G.add_vertex nodesr v) vr ;
  List.iter (fun e -> G.add_vertex edgesr e) er ;*)
  let lhs = {nodes= nodesl; edges = edgesl} in
  let rhs = {nodes= nodesr; edges = edgesr} in
  {id=inc(); src = sub; dst = over ;lhs; rhs}

let empty src dst =
  {id= inc (); src; dst; lhs= empty_inc (); rhs= empty_inc ()}

let setNode_rhs t id1 id2 =
  G.add_edge t.rhs.nodes id1 id2 ;
  t

let setNode_lhs t id1 id2 =
  G.add_edge t.lhs.nodes id1 id2 ;
  t

let setEdges_lhs t id1 id2 =
  let v1 = G.find_vertex t.lhs.edges id1 in
  let v2 = G.find_vertex t.lhs.edges id2 in
  G.add_edge t.lhs.edges v1 v2 ;
  t

let setEdges_rhs t id1 id2 =
  G.add_edge t.rhs.edges id1 id2 ;
  t

let unset_node_rhs t v =
  G.remove_vertex t.rhs.nodes v ;
  t

let unset_node_lhs t v =
  G.remove_vertex t.rhs.nodes v ;
  t

let unset_edge_lhs t e =
  G.remove_vertex t.lhs.edges e ;
  t

let unset_edge_rhs t e =
  G.remove_vertex t.rhs.edges e ;
  t

let print t =
  Format.printf "\n Left \n Nodes inclusion: \n" ;
  G.dot_output t.lhs.nodes ("nodesleft" ^ string_of_int t.id ^ ".dot") ;
  Format.printf "\nEdges Inclusion: \n" ;
  G.dot_output t.lhs.edges ("edgeleft" ^ string_of_int t.id ^ ".dot") ;
  Format.printf "\n Right \n Nodes inclusion: \n" ;
  G.dot_output t.rhs.nodes ("nodesright" ^ string_of_int t.id ^ ".dot") ;
  Format.printf "\n Edges Inclusion: \n" ;
  G.dot_output t.rhs.edges ("edgeright" ^ string_of_int t.id ^ ".dot")

module G = Graph.Pack.Digraph

module Rule = struct
  type hs = G.t

  let lcpt = ref 0

  let linc () =
    ignore (lcpt := !lcpt + 1) ;
    !lcpt

  let rcpt = ref 0

  let rcpt_e = ref 0

  let lcpt_e = ref 0

  let rinc () =
    ignore (rcpt := !rcpt + 1) ;
    !rcpt

  let rinc_e () =
    ignore (rcpt_e := !rcpt_e + 1) ;
    !rcpt_e

  let linc_e () =
    ignore (lcpt_e := !lcpt_e + 1) ;
    !lcpt_e

  type t = {id: int; lhs: hs; rhs: hs}

  (*let empty () = {id= 0; lhs= G.create (); rhs= G.create ()}*)
  let empty id = {id; lhs= G.create (); rhs= G.create ()}

  let add_edge_lhs t v1 v2 =
    let e = G.E.create v1 (linc_e ()) v2 in
    G.add_edge_e t.lhs e;
    t

  let add_edge_rhs t v1 v2 =
    let e = G.E.create v1 (rinc_e ()) v2 in
    G.add_edge_e t.rhs e;
    t

  let add_vertex_lhs t =
    let v = G.V.create (linc ()) in
    G.add_vertex t.lhs v ; v

  let add_vertex_rhs t =
    let v = G.V.create (rinc ()) in
    G.add_vertex t.rhs v ; v

  let print t =
    G.dot_output t.rhs ("rhs" ^ string_of_int t.id ^ ".dot") ;
    G.dot_output t.lhs ("lhs" ^ string_of_int t.id ^ ".dot")
end

module RuleSystem = struct
  module Rmap = Map.Make (G.V)
  module Emap = Map.Make (G.E)

  let rcpt = ref 0

  let icpt = ref 0

  let inc v =
    v := !v + 1 ;
    !v

  let rinc () = inc rcpt

  let iinc () = inc icpt

  type t = {graph: G.t; rules: Rule.t Rmap.t; inc: Inclusion.t Emap.t}

  let empty () = {graph= G.create (); rules= Rmap.empty; inc= Emap.empty}

  let create_inclusion t sub over =
    G.add_edge t.graph sub over ;
    let e = G.find_edge t.graph sub over in
    let i =
      Inclusion.create
        (G.fold_vertex (fun v l -> v :: l) (Rmap.find sub t.rules).lhs [])
        (G.fold_vertex (fun v l -> v :: l) (Rmap.find over t.rules).lhs [])
        (G.fold_edges_e (fun v l -> v :: l) (Rmap.find over t.rules).lhs [])
        (G.fold_edges_e (fun v l -> v :: l) (Rmap.find sub t.rules).lhs [])
        (G.V.hash sub) (G.V.hash over)
    in
    ({t with inc= Emap.add (G.find_edge t.graph sub over) i t.inc}, e)

  let get_rule t v = Rmap.find v t.rules

  let get_inclusion t e = Emap.find e t.inc

  let update_inclusion t v r =
    {t with inc= Emap.update v (fun _ -> Some r) t.inc}

  let update_rule t v r =
    {t with rules= Rmap.update v (fun _ -> Some r) t.rules}

  let create_rule t =
    let key = rinc () in
    Format.printf "Create key %d" key ;
    let v = G.V.create key in
    ({t with rules= Rmap.add v (Rule.empty key) t.rules}, v)

  let delete_rule t v =
    let edges = G.succ_e t.graph v @ G.succ_e t.graph v in
    G.remove_vertex t.graph v ;
    let rules = Rmap.remove v t.rules in
    let inc = List.fold_left (fun map e -> Emap.remove e map) t.inc edges in
    {graph= t.graph; rules; inc}

  let delete_inclusion t e =
    G.remove_edge t.graph (G.E.src e) (G.E.src e) ;
    {t with inc= Emap.remove e t.inc}

  let print t =
    Format.printf "RulesSystem\n" ;
    G.dot_output t.graph "rs.dot" ;
    let _ = Rmap.map (fun r -> Rule.print r) t.rules in
    Emap.map (fun i -> Inclusion.print i) t.inc
end

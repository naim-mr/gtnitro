open Gtnitro
open Rules
module RS = Gtnitro.Rules.Rule
module G = Graph.Pack.Digraph

let _ =
  let rs = RuleSystem.empty () in
  let rs, v1 = RuleSystem.create_rule rs in
  let rs, v2 = RuleSystem.create_rule rs in
  let r1 = RuleSystem.get_rule rs v1 in
  let r2 = RuleSystem.get_rule rs v2 in
  let lv1, lv2, rv1 =
    (Rule.add_vertex_lhs r1, Rule.add_vertex_lhs r1, Rule.add_vertex_rhs r1)
  in
  let r1 = Rule.add_edge_lhs r1 lv1 lv2 in
  let lv21, lv22, lv23 =
    (Rule.add_vertex_lhs r2, Rule.add_vertex_lhs r2, Rule.add_vertex_lhs r2)
  in
  let rv21, rv22 = (Rule.add_vertex_rhs r2, Rule.add_vertex_rhs r2) in
  let r2 =
    Rule.add_edge_lhs r2 lv21 lv22
    |> fun r2 ->
    Rule.add_edge_lhs r2 lv22 lv23
    |> fun r2 -> Rule.add_edge_rhs r2 rv21 rv22
  in  
  let rs = RuleSystem.update_rule  rs v1 r1 in 
  let rs = RuleSystem.update_rule  rs v2 r2 in 
  let rs,e = RuleSystem.create_inclusion rs v1 v2 in 
  let inc = RuleSystem.get_inclusion rs e |> fun inc ->  Inclusion.setNode_lhs  inc lv1 lv21 
          |> fun inc ->  Inclusion.setNode_lhs  inc lv2 lv22
          |> fun inc ->  Inclusion.setNode_rhs  inc rv1 rv21 
          |> fun inc ->  Inclusion.setEdges_lhs  inc (G.E.label (G.find_edge  r1.lhs lv1 lv2))  (G.E.label (G.find_edge r2.lhs lv21 lv22)) in
  let rs = RuleSystem.update_inclusion rs e inc in 
  RuleSystem.print rs

  
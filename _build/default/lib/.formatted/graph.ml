type graph = { nodes : int list; edges : (int*int) list }

let ncounter = ref (-1)


    let increment_ncounter () = ncounter := !ncounter + 1 
let add_nodes g = 
  ignore(increment_ncounter ());
  { g with nodes = !ncounter::g.nodes }


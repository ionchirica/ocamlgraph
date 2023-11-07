open Graph
open Printf

module Int = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0
end

module G = Imperative.Digraph.Concrete(struct
               type t = int
               let compare = compare
               let hash = Hashtbl.hash
               let equal = (=) end)

module R = Rand.I(G)
module O = Oper.I(G)
let test_intersect g g' =
  let inter_graph_membership = O.intersect g g' in
  let inter_list_membership = O.intersect_old g g' in

  let nb_vert_l = G.nb_vertex inter_list_membership in
  let nb_vert_m = G.nb_vertex inter_graph_membership in

  let nb_edge = G.nb_edges inter_graph_membership in
  let ne int = G.fold_edges_e
    (
      fun e ne -> if (G.mem_edge_e g e) && (G.mem_edge_e g' e)
                  then ne + 1
                  else ne
    ) int 0 in
  printf "intersection_old number of edges %d\n" nb_edge;
  printf "number of vertices mem %d\n" nb_vert_m;
  printf "number of vertices list %d\n" nb_vert_l;
  printf "number of edges %d\n" (ne inter_graph_membership);
  printf "number of edges %d\n" (ne inter_list_membership);
  assert (nb_edge = ne inter_graph_membership );
  assert (nb_edge = ne inter_list_membership );
  assert ( nb_vert_l = nb_vert_m );
  ()

let seed_ = ref None
let seed = match !seed_ with
  | None -> Random.self_init (); Random.int (1 lsl 29)
  | Some s -> s

let test_random =
  Random.init seed;
  let r1 = R.graph ~v: 10 ~e: 7 ()in
  let r2 = R.graph ~v: 10 ~e: 7 ()in

  printf "Testing intersection of two random graphs\n";
  test_intersect r1 r2;
  printf "\n";

  printf "Testing intersection of the same graph\n";
  test_intersect r1 r1;
  printf "\n";
  ()

let () =
  let g = G.create () in
  let g2 = G.create () in
  printf "Testing intersection of empty graphs\n";
  test_intersect g g2;    (* {} n {} -> {} *)
  printf "\n";

  let v1 = G.V.create 1 in
  let v2 = G.V.create 2 in
  let v3 = G.V.create 3 in
  G.add_edge g v1 v2;
  G.add_edge g v2 v1;
  G.add_edge g v2 v3;
  printf "Testing intersection of a non empty graph with empty graph\n";
  test_intersect g g2;    (* {{1, 2}, {2, 1}, {2, 3}} n {} -> {} *)
  printf "\n";
  printf "Testing intersection of a empty graph with a non empty graph\n";
  test_intersect g2 g;    (* {{1, 2}, {2, 1}, {2, 3}} n {} -> {} *)
  printf "\n";

  G.add_edge g2 v1 v2;
  G.add_edge g2 v1 v3;
  G.add_edge g2 v2 v1;
  printf "Testing intersection of a common edges\n";
  test_intersect g g2;    (* {{1, 2}, {2, 1}, {2, 3}} n                   *)
  printf "\n";            (* {{1, 2}, {1, 3}, {2, 1}} -> {{1, 2}, {2, 1}} *)

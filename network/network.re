module SS = Set.Make(String);

[@deriving (show, yojson)]
type network = {nodes: list(string)};

let register_nodes = (network, addresses) => {
  open SS;
  let next_nodes = union(of_list(network.nodes), of_list(addresses));
  {nodes: next_nodes |> to_seq |> List.of_seq};
};

let create = () => {nodes: []};

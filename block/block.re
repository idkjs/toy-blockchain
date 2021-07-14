open Transaction;

[@deriving (show, yojson)]
type block = {
  index: int,
  timestamp: float,
  proof: int,
  previous_hash: string,
  transactions: list(transaction),
};

let hash = b => b |> show_block |> Sha256.string |> Sha256.to_hex;

let genesis = () => {
  index: 0,
  timestamp: Unix.gettimeofday(),
  transactions: [],
  proof: 0,
  previous_hash: "",
};

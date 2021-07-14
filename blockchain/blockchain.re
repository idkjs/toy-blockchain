open Block;
open Transaction;
open Network;

[@deriving (show, yojson)]
type blockchain = {
  chain: list(block),
  pending: list(transaction),
};

let add_block = ({chain, pending}, proof) =>
  switch (chain) {
  | [] => Error("No previous block")
  | [previous_block, ..._] =>
    let new_block = {
      index: List.length(chain),
      timestamp: Unix.gettimeofday(),
      proof,
      previous_hash: hash(previous_block),
      transactions: pending,
    };
    Ok({chain: [new_block, ...chain], pending: []});
  };

let add_transaction = ({chain, pending}, transaction) =>
  Ok({chain, pending: [transaction, ...pending]});

let create = () => {chain: [Block.genesis()], pending: []};

let valid_proof = (proof, last_proof) => {
  let guess = Printf.sprintf("%d%d", last_proof, proof);
  let guess_hash = guess |> Sha256.string |> Sha256.to_hex;
  String.sub(guess_hash, 0, 4) == "0000";
};

let proof_of_work = last_proof => {
  let rec loop = proof =>
    if (valid_proof(proof, last_proof)) {
      proof;
    } else {
      loop(proof + 1);
    };
  loop(0);
};

let mine = ({chain, pending}, node_id) =>
  if (List.length(pending) == 0) {
    Error("No pending transactions to mine");
  } else {
    switch (chain) {
    | [] => Error("No previous block")
    | [previous_block, ..._] =>
      let proof = proof_of_work(previous_block.proof);
      let reward: transaction = (
        {sender: "0", recipient: node_id, amount: 1}: transaction
      );
      switch (add_transaction({chain, pending}, reward)) {
      | Ok(blockchain') => add_block(blockchain', proof)
      | Error(message) => Error(message)
      };
    };
  };

let rec valid_chain = chain =>
  switch (chain) {
  | [] => true
  | [first] => first.previous_hash == ""
  | [first, second, ...rest] =>
    first.previous_hash == Block.hash(second)
    && valid_proof(first.proof, second.proof)
    && valid_chain([second, ...rest])
  };

let max_by = (get_score, ls) => {
  let rec loop = (max_score, max_item, todo) =>
    switch (todo) {
    | [] => max_item
    | [head, ...tail] =>
      let score = get_score(head);
      if (score > max_score) {
        loop(score, head, tail);
      } else {
        loop(max_score, max_item, tail);
      };
    };
  switch (ls) {
  | [] => None
  | [head, ...tail] => Some(loop(get_score(head), head, tail))
  };
};

let resolve_conflicts = (current_blockchain, network, get_neighbour) => {
  open Lwt;
  let current_length = List.length(current_blockchain.chain);
  let neighbour_addresses = network.nodes;

  Lwt_list.map_p(get_neighbour, neighbour_addresses)
  >>= (
    results => {
      let neighbours = List.filter_map(Result.to_option, results);
      return(
        switch (max_by(b => List.length(b.chain), neighbours)) {
        | Some(longest) =>
          if (List.length(longest.chain) > current_length
              && valid_chain(longest.chain)) {
            (
              {chain: longest.chain, pending: current_blockchain.pending},
              true,
            );
          } else {
            (current_blockchain, false);
          }
        | None => (current_blockchain, false)
        },
      );
    }
  );
};

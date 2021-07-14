[@deriving (show, yojson)]
type transaction = {
  sender: string,
  recipient: string,
  amount: int,
};

open BoardStuffs

(* A threat consists of a gain square, the rest squares, and cost squares *)
type threat = Threat of index * index list * index list

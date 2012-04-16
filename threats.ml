open Board
open Threat

(*This is the file that will implement threat spaced search *)

exception TODO;;

(* you have a board at the top of the tree
   call get_threats on the board and you are given all the potential threats

   each node other than the top node is going to be a threat and a board


*)


module type THREATS = 
sig
  
  type board
  
  (* one threat *)
  type threat
  
  (* a collection of threats *)
  type threats
  
  type tree
  
  val empty : tree

  (* Given a board returns the threats associated with it *)
  val get_threats: board -> threats
  
  (* Given a collection of threats xs and a threat y 
     returns the threats in xs that are dependent on y *)
  val dependent_threats : threats -> threat -> threat

  (* Given a board and the threat whose gain square was the last move
     made on the board returns all threats dependent on the given threat*)
  val get_dependent_threats : board -> threat -> threats

  (* Given an old board and a threat generates the board that would result if
     black played the gain square and white played all the cost squares *)  
  val gen_new_board: board -> threat -> board

  (* Given a board and the threat whose gain square was the last move
     made on the board returns the threat tree from that board *)
  val gen_threat_tree : board -> threat -> tree
  
  (* Evaluates the tree to see if it results in a winning threat sequence *)
  val evaluate_tree : tree -> bool

  (* Merges two independent trees into one tree *)  
  val merge : tree -> tree -> tree
end
  
functor (B: BOARD) -> 
  struct
    type board = B.board
    type threat = threat
    type threats = threat list
    type tree = 
      | Node of board * threat * (tree list)
      | Leaf of board * threat   
      | Win of board * threat 

    let get_threats = B.getThreats

    let dependent_threats (ts: threats) (t: threat) = 
      let (tgain, _, _) = t in
      let dependent x = 
        let (_ , trest, _) = x in
        List.exists (fun y -> (tgain = y)) trest in
      List.filter dependent threats 

    let get_dependent_threats (b: board) (t: threat) =
      dependent_threats (get_threats b) t

    let gen_new_board (b: board) (t:threat) =
      let (tgain, _, tcost) = t in
      let rec insertwhitelist (b:board) (t: index list) =
        match t with
        | hd::tl -> insertlist (board.insert b hd White) tl
        | _ -> b in
      insertwhitelist (board.insert b tgain Black) tcost

    let gen_threat_tree (b: board) (t: threat) = 
      if B.iswin then
	Win(b, t)
      else
	let threatList = get_dependent_threats b t in 
	if threatlist = [] then 
	  Leaf(b, t)	    
	else
	  let boardList = List.map (fun x -> (gen_new_board b x), x) threatList in


(*
      let treeList = List.map (fun x -> (Leaf(gen_new_board b x), x)) threatList in
      match treeList with
      | hd::_ -> Node(b, t, treeList)
      | [] -> Leaf(b, t)
*)

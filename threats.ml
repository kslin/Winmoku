open Board
open Boardstuffs

(*This is the file that will implement threat spaced search *)

module type THREATS = 
sig
  
  type board
  
  (* one threat *)
  type threat
  
  (* a collection of threats *)
  type threats
  
  type tree

  (* Given a board returns the threats associated with it *)
  val get_threats: board -> threats
  
  (* Given a collection of threats xs and a threat y 
     returns the threats in xs that are dependent on y *)
  val dependent_threats : threats -> threat -> threats

  (* Given a board and the threat whose gain square was the last move
     made on the board returns all threats dependent on the given threat*)
  val get_dependent_threats : board -> threat -> threats

  (* Given an old board and a threat generates the board that would result if
     black played the gain square and white played all the cost squares *)  
  val gen_new_board: board -> threat -> board

  (* Given a board and the threat whose gain square was the last move
     made on the board returns the threat tree from that board *)
  val gen_threat_tree : board -> threat -> threats -> tree
  
  (* Evaluates the tree to see if it results in a winning threat sequence *)
  val evaluate_tree : tree -> threats option

  (* Outputs the next threat if there is a winning threat sequence *)
  val next_winning_move : tree -> threat option

  (* Merges two independent trees into one tree *)  
  val merge : tree -> tree -> tree

end

module TGenerator(B: BOARD):THREATS with type board = B.board 
                                    and type threat = Boardstuffs.threat 
                                    and type threats = threat list =
  struct
    type board = B.board
    type threat = Boardstuffs.threat
    type threats = threat list
    type tree = 
      | Node of board * threat * (tree list)
      | Leaf of board * threat   
      | Win of board * threat * threats    

    let get_threats = B.getThreats

    let dependent_threats (ts: threats) (t: threat) = 
      let Threat(_, tgain, _, _) = t in
      let dependent x = 
        let Threat(_, _, _, trest) = x in
        List.exists (fun y -> (tgain = y)) trest in
      List.filter dependent ts 

    let get_dependent_threats (b: board) (t: threat) =
      dependent_threats (get_threats b) t

    let gen_new_board (b: board) (t:threat) =
      let Threat(_, tgain, tcost, _) = t in
      let rec insertwhitelist (b:board) (t: index list) =
        match t with
        | hd::tl -> insertwhitelist (B.insertspecial b hd White) tl
        | _ -> b in
      insertwhitelist (B.insertspecial b tgain Black) tcost

    let rec gen_threat_tree (b: board) (t: threat) (tlist: threats) = 
      let Threat(ttype, _, _, _) = t in
      if (B.isWin b != None) || (ttype = StraightFour) then 
        Win(b, t, t::tlist)
      else 
        let threatList = get_dependent_threats b t in 
          if threatList = [] then 
            Leaf(b, t)      
          else
            let tree_from_threat (x:threat) = 
              gen_threat_tree (gen_new_board b x) x (t::tlist)
            in
            let treeList = 
              List.map tree_from_threat threatList 
            in
              Node(b, t, treeList)
    
    let rec evaluate_tree (tr: tree) =
      let rec evaluate_tree_list treelist =
        match treelist with
        | [] -> None
        | hd::tl ->
	  let result = evaluate_tree hd in
	  (if result = None then (evaluate_tree_list tl)
                     else result)
      in
        match tr with
        | Win(b, t, tlist) -> Some tlist
        | Leaf(b, t) -> None 
        | Node(b, t, treeList) -> (evaluate_tree_list treeList)

    let next_winning_move (tr: tree) = 
      match evaluate_tree tr with
	| None -> None
	| Some _ ->
	  (match tr with
	    | Win(b, t, tlist) -> Some t
	    | Leaf(b, t) -> None
	    | Node(b, t, tlist) -> Some t)

    let rec merge tree1 tree2 = tree1
end

module BThreats = TGenerator(Myboard)

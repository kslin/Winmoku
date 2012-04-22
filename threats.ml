open Board
open Threat

(*This is the file that will implement threat spaced search *)

exception TODO;;

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
  
module tGenerator(B: BOARD) : THREATS =  
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
        | hd::tl -> insertlist (B.insert b hd White) tl
        | _ -> b in
      insertwhitelist (B.insert b tgain Black) tcost

    let rec gen_threat_tree (b: board) (t: threat) = 
      if (B.isWin b) then 
        Win(b, t)
      else
        let threatList = get_dependent_threats b t in 
          if threatlist = [] then 
            Leaf(b, t)      
          else
            let tree_from_threat (x:threat) = 
              gen_threat_tree (gen_new_board b x) x
            in
            let treeList = 
              List.map tree_from_threat threatList 
            in
              Node(b, t, treeList)
    
    let rec evaluate_tree (tr: tree) =
      let rec evaluate_tree_list treelist =
        match treelist with
        | [] -> false
        | hd::tl -> (evaluate_tree hd) || (evaluate_tree_list tl)
      in
        match tr with
        | Win(b, t) -> True 
        | Leaf(b, t) -> False 
        | Node(b, t, treeList) -> (evaluate_tree_list treeList)

    let rec merge tree1 tree2 = tree1
end

module BThreats = tGenerator(Board)

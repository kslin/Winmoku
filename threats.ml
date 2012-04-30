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

  (* Given a board and the threat whose gain square was the last move
     made on the board, return all four threats dependent on given threat *)
  val get_dependent_four_threats : board -> threat -> threats

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

  (* Evaluates a board, and returns a winning sequence if there is one *)
  val evaluate_board : board -> threats option

  (* On a board with no threats, identify potential moves by searching for
   * hidden threats. *)
  val hidden_threats : board -> index list

end

module TGenerator(B: BOARD):THREATS with type board = B.board 
                                    and type threat = Boardstuffs.threat 
                                    and type threats = threat list =
  struct
    type board = B.board
    type threat = Boardstuffs.threat
    type threats = threat list

    (* Node: holds a board, the threat that led to the board, and a list
       of all trees arising from threats on the board 
       and dependent on the given threat - corresponds to a threat
       in a threat sequence
       Leaf: holds a board and the threat that led to the board - 
       corresponds to a board that has no more dependent threats
       Win: holds a board, the threat that led to the board, and a 
       collection of all the threats that led up to the winning move -
       corresponds to a board with an unblockable threat and holds the
       winning sequence *)

    type tree = 
      | Node of board * threat * (tree list)
      | Leaf of board * threat   
      | Win of board * threat * threats    
      | Loss

    
    let get_threats = B.getThreats

    (* A threat A is dependent on a threat B if the gain square of B
       is one of the rest squares of A *)
    let dependent_threats (ts: threats) (t: threat) = 
      let Threat(_, tgain, _, _) = t in
      let dependent x = 
        let Threat(_, _, _, trest) = x in
        List.exists (fun y -> (tgain = y)) trest in
      List.filter dependent ts 

    let get_dependent_threats (b: board) (t: threat) =
      dependent_threats (get_threats b) t

    (* Given a board and a threat t, returns all threats on the board
       dependent on t and are of type StraightFour or Four. 
       These two threats are different because they result in a win
       on the next move. *)
    let get_dependent_four_threats (b: board) (t: threat) = 
      List.filter
	(fun x -> 
	  let Threat(ttype, _, _, _) = x in
	  ttype = StraightFour || ttype = Four)
	(get_dependent_threats b t)

    (* Given a board and a threat t, returns the board after black
       has played the gain square of the threat and white has played
       all the cost squares of the threat *)
    let gen_new_board (b: board) (t:threat) =
      let Threat(_, tgain, tcost, _) = t in
      let rec insertwhitelist (b:board) (t: index list) =
        match t with
        | hd::tl -> insertwhitelist (B.insertspecial b hd White) tl
        | _ -> b in
      insertwhitelist (B.insertspecial b tgain Black) tcost

    (* Given a board and a threat t, returns the threat tree created by
       recursively finding all threats dependent on t *)
    let rec gen_threat_tree (b: board) (t: threat) (tlist: threats) = 
      let Threat(ttype, _, _, _) = t in
      if (B.isWin b = Some Black) || (ttype = StraightFour) then 
        Win(b, t, t::tlist)
      else if (B.isWin b = Some White) || 
	(match B.nextWhiteWin b with | None -> false | Some _ -> true)
      then 
	Loss
      else
	(* if White is able to create a Three, then Black can only search for
	   StraightFour or Four threats because only those threats have
	   higher priority than White's Three *) 
	let threatList = 
	  if List.exists 
	    (fun wt -> let Threat(wttype,_,_,_) = wt in wttype = StraightFour)
	    (B.getWhiteThreats b) 
          then
	    get_dependent_four_threats b t
	  else
	    get_dependent_threats b t
	in
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
	  
    (* Given a tree, if the tree has a winning threat sequence then
       returns the threat sequence leading to the win *)
    let rec evaluate_tree (tr: tree) : threats option =
      let rec evaluate_tree_list treelist =
        match treelist with
        | [] -> None
        | hd::tl -> (let result = evaluate_tree hd in
	                     if result = None then (evaluate_tree_list tl)
                       else result)
      in
        match tr with
        | Win(b, t, tlist) -> Some tlist
        | Leaf(b, t) -> None 
        | Loss -> None 
        | Node(b, t, treeList) -> (evaluate_tree_list treeList)

    (* Given a tree, if there is a winning threat sequence returns the
       first threat of that sequence *)
    let next_winning_move (tr: tree) = 
      match evaluate_tree tr with
	    | None -> None
	    | Some _ ->
	      (match tr with
	       | Win(b, t, tlist) -> Some t
	       | Leaf(b, t) -> None
	       | Node(b, t, tlist) -> Some t
	       | Loss -> None
	      )

    let rec merge tree1 tree2 = 
      (* grabs first threat of tree2 if that threat is independent *)
      (* let within_five origin point : bool = 
	      let x0, y0 = origin in
	      let x1, y1 = point in
	      let dx = abs (x1-x0) in
	      let dy = abs (y1-y0) in
	      (x0 = x1 && dy < 5) ||
	      (y0 = y1 && dx < 5) ||
	      (dx = dy && dx < 5)  
      in *)
      (* within_five was meant to restrict merging so that only threats
	 close to each other were merged to improve performance. 
	 However, we could miss possible winning sequences.  Since
	 performance of merge was not horrible we left it out *)
      let traverse2 (costs1: index list) (tgain: index) : threat option = 
	match tree2 with
	  | Win(_, _, _) -> raise Boardstuffs.ERROR
	  | Node(_, t, _) | Leaf(_, t) ->
	    let Threat(_, tgain2, tcost2, _) = t in
	    if 
	      (* (within_five tgain tgain2) || *)
	      (List.fold_left 
	      (fun result index -> (List.mem index costs1) || result)
	      false (tgain2::tcost2)) then None
	    else Some(t)
	  | Loss -> None
      in
      (* iterates through tree1, for each node and leaf it makes a tree from
         the threat in tree2 (if independent) and adds it to treeList *)
      let rec traverse1 costs threatlist tree1 = 
	match tree1 with
	  | Win(_, _, _) -> raise Boardstuffs.ERROR
	    (* Leaf is turned into a Node if it can be merged *)
	  | Leaf(b, t) -> 
	    let Threat(_, tgain, tcost, _) = t in
	    let new_costs = costs @ tcost in
	    (match traverse2 new_costs tgain with
	      | None -> Leaf(b, t)
	      | Some new_t -> 
		let new_tree = 
		  gen_threat_tree (gen_new_board b new_t) new_t (t::threatlist)
		in
		Node(b, t, [new_tree]))
	    (* Nodes remain as nodes but gain an addition tree in tlist *)
	  | Node(b, t, tlist) ->
	    let Threat(_, tgain, tcost, _) = t in
	    let new_costs = costs @ tcost in
	    (match traverse2 new_costs tgain with
	      | None -> Node(b, t, tlist)
	      | Some new_t ->
		let new_tree = 
		  gen_threat_tree (gen_new_board b new_t) new_t (t::threatlist)
		in
		let new_tlist = 
		  List.map (traverse1 new_costs (t::threatlist)) tlist in
		Node(b, t, new_tree::new_tlist))
	  | Loss -> Loss
      in
      traverse1 [] [] tree1 
    
    (* Given a board, returns a sequence of winning threats if possible *)
    let evaluate_board board : threats option = 
      let threatlist = get_threats board in
      let update_board threat = ((gen_new_board board threat), threat) in 
      let boardlist = List.map update_board threatlist in
      let treelist = List.map (fun (x, y) -> (gen_threat_tree x y [])) 
                              boardlist in 
      let rec win tlist =   
        match tlist with 
        | [] -> None
        | hd::tl -> (if evaluate_tree hd = None then (win tl)
                     else evaluate_tree hd)
      in
      (* if no threat tree contains a winning threat sequence then
	 merge each tree with each other tree to see if the merged trees
	 contain winning sequences - corresponds to playing two 
	 independent threats *)
      match win treelist with
	| None ->
	  let mergefunctionlist = List.map merge treelist in
	  let mergetreelist = 
	    List.flatten (
	      List.map (fun f -> List.map f treelist) mergefunctionlist)
	  in
	  win mergetreelist
	| x -> x


    (* Check if index is worth evaluating for hidden_threats *)
    let check_index (b: board) (i: index) : bool = 
      if (B.get b i) == Unocc then 
        let coords = Boardstuffs.indices_within 3 i in
        let rec count_color (ilist: index list) (color: occupied) =
          match ilist with
          | [] -> 0
          | hd::tl -> (if (B.get b hd) == color then 1 + (count_color tl color)
                       else (count_color tl color))
        in
          if (count_color coords Black) + (count_color coords White) = 0 then 
            false
          else 
            if get_threats (B.insertspecial b i Black) != [] then true
            else false  
      else false 

    let hidden_threats (b: board) =
      let range = Boardstuffs.range 0 (Boardstuffs.world_size -1) in
      let coords = Boardstuffs.cross range range in
      let candidates = List.filter (check_index b) coords in 
      let candidateboards = List.map 
                              (fun x -> ((B.insertspecial b x Black), x)) 
                              candidates 
      in 
      let hiddenboards = List.filter (fun (x,y) -> if evaluate_board x = None
                                                   then false
                                                   else true)
                                      candidateboards 
      in
        List.map snd hiddenboards
end

module BThreats = TGenerator(Myboard)

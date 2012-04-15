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
  
functor (board: BOARD) -> 
  struct
    type board = board.board
    type threat = threat
    type threats = threat list
      
    let get_threats = Board.getThreats

    let get_dependent_threats (b: board) (t: threat) =
      bOld = Board.remove b t in 
      nThreats = get_threats b in
      oThreats = get_threats bOld in
      let rec notin s1 s2 = 
	match s2 with
        | hd::tl -> if List.exists (fun x -> (x = hd)) s1 then notin s1 td 
                    else hd::(notin s1 td)
        | _ -> [] in
      notin oThreats nThreats

    let gen_new_board (b: board) (t:threat) =
      
       
         

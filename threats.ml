(*This is the file that will implement threat spaced search" *)

exception TODO

(* you have a board at the top of the tree
   call get_threats on the board and you are given all the potential threats

   each node other than the top node is going to be a threat and a board


*)

module THREATS = 
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
  val generate_new_board: board -> threat -> board

  (* Given a threat adds a threat to the tree and returns the new tree*)
  val add_threat : tree -> threat -> tree
  
  (* Evaluates the tree results in a winning threat sequence *)
  val evaluate_tree : tree -> bool

  (* Merges two independent trees into one tree *)  
  val merge : tree -> tree -> tree

end

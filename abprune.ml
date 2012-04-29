open Board
open Boardstuffs
open Threats
(* Module for alpha-beta pruning algorithm, modified to suit gomoku. *)

module type ABPRUNER = 
sig
  
  type board
  
  (* one threat *)
  type threat
  
  (* a collection of threats *)
  type threats
  
  type tree

end

module PGenerator(T: THREATS) with type board = T.board 
                              and type threat = T.threat 
                              and type threats = threat list =
  struct
    type board = T.board
    type threat = T.threat
    type threats = threat list
    type tree = 
      | Node of board * threat * (tree list)
      | Leaf of board * threat   
      | Win of board * threat * threats    
      | Loss
end

module GomokuPruner = PGenerator(BThreats)

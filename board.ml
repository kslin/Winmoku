(* This file contains all the modules and functions for the Gomoku board *)
exception TODO

(* This is a signature for a Gomoku board *)
module BOARD =
sig
    type board
    type place
    type color
    type threats

    (* An empty board *)
    val empty: board

    (* Given an index, returns whether it is occupied and with which color*)
    val getIndex: place -> color

    (* Checks if a space exists and is empty *)
    val isValid: board -> place -> bool

    (* Inserts pieces on the board *)
    val insert: board -> place -> color -> board

    (* Removes pieces from a board, returns None if the remove is invalid 
       This is only used for testing purposes *)
    val remove: board -> place -> board

    (* Given an index, returns a list of neighbors *)
    val getNeighbors: place -> place list

    (* Checks if the board has a winning configuration *)
    val isWin: board -> bool

    (* Given a board, returns all the threats *)
    val getThreats: board -> threats
    
end

(* This is a signature for a board that only considers specific neighbors
   For example, a board of only the horizontal relationships *)
module MiniBoard (BT: boardtype) (size: int) : BOARD =
struct
    Open Occupied
    Open Threat
    Open Boardstuffs
    type board = occupied list list
    type place = index
    type color = occupied
    type threats = threat list

    (* An empty board *)
    let empty = []
 
    (* determines if an attempted insert is valid *)
    let isValid (i: index) : bool = raise TODO

    (* Inserts a piece on the board *)
    let insert (b: board) (i: index) (c: color) : option = raise TODO

    (* Removes a piece on the board *)
    let remove (b: board) (i: index) : board = raise TODO

    (* Gets neighbors but only in the direction specified by BT *)
    let getNeighbors (i: index) : index list = raise TODO

    (* Checks if there is a win in the direction specified by BT *)
    let isWin (b: board) : bool = raise TODO

    (* Gets the threats in the direction specified *)
    let getThreats (b: board) : threats = raise TODO

    (* *)
    
end
(* This file contains all the modules and functions for the Gomoku board *)
exception TODO

(* This is a signature for a Gomoku board *)
module BOARD =
sig
    type board
    type index
    type color
    type threat

    (* An empty board *)
    val empty: board

    (* Given an index, returns whether it is occupied and with which color*)
    val getIndex: index -> color

    (* Inserts pieces on the board, returns None if the insert is invalid *)
    val insert: board -> index -> color -> board option

    (* Removes pieces from a board, returns None if the remove is invalid *)
    val remove: board -> index -> board option

    (* Given an index, returns a list of neighbors *)
    val getNeighbors: index -> index list

    (* Checks if the board has a winning configuration *)
    val isWin: board -> bool

    (* Given a board, returns a list of threats *)
    val returnThreats: board -> threat list
    
end

(* This is a signature for a board that only considers specific neighbors
   For example, a board of only the horizontal relationships *)
module MiniBoard : BOARD =
struct
    Open Color
    type board
    type index 
    type color

    (* An empty board *)
    val empty: board

    (* Inserts a piece on the board *)
    val insert: board -> index -> board

    (* Removes a piece on the board *)
    val remove: 


    
end
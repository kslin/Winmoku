(* This file contains all the modules and functions for the Gomoku board *)
exception TODO

open Boardstuffs
open Occupied
open Threat

let unisize = 19


(* This is a signature for a Gomoku board *)
module type BOARD =
sig
    type board
    type place
    type color
    type threats

    (* An empty board *)
    val empty: board

    (* Given an index, returns whether it is occupied and with which color*)
    val getIndex: board -> place -> color

    (* Checks if a space exists and is empty *)
    val isValid: board -> place -> bool

    (* Inserts pieces on the board *)
    val insert: board -> place -> color -> board option

    (* Removes pieces from a board, returns None if the remove is invalid 
       This is only used for testing purposes *)
    val remove: board -> place -> board

    (* Given an index, returns a list of neighbors *)
    val getNeighbors: board -> place -> place list

    (* Checks if the board has a winning configuration *)
    val isWin: board -> bool

    (* Given a board, returns all the threats *)
    val getThreats: board -> threats
    
end

module type BOARD_ARG = 
sig
    
    val btype : boardtype
    
    val convertIndex : index -> int*int

end

module HorizontalBoardArg =
struct

    let btype = Horizontal

    let convertIndex (i: index) = i
end

module VerticalBoardArg =
struct

    let btype = Vertical

    (* switch the coordinates *)
    let convertIndex (i: index) = let (x,y) = i in (y,x)
end

module DiagRightBoardArg =
struct

    let btype = DiagRight

    let convertIndex (i: index) = 
        let (x,y) = i in 
        if y > x then (unisize - (y-x), x)
        else (unisize + (x-y), y)
end

module DiagLeftBoardArg =
struct

    let btype = DiagLeft

    let convertIndex (i: index) = 
        let (x,y) = i in
        if (x + y - 1) <= unisize then (x + y - 1, y)
        else (x + y - 1, y + 1 - (x + y - unisize)) 
end
(* This is a signature for a board that only considers specific neighbors
   For example, a board of only the horizontal relationships *)
module MiniBoard (B: BOARD_ARG) : BOARD =
struct

    type board = (index * occupied) list
    type place = index
    type color = occupied
    type threats = threat list

    let empty = [] 

    let isValid (b: board) (i: index) : bool = raise TODO

    let getIndex (b: board) (p: place) : color = raise TODO

    let insert (b: board) (i: index) (c: color) : board option = raise TODO

    let remove (b: board) (i: index) : board = raise TODO

    let getNeighbors (b: board) (i: index) : index list = raise TODO

    let isWin (b: board) : bool = raise TODO

    let getThreats (b: board) : threats = raise TODO
    
end

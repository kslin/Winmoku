exception TODO
exception ERROR

open Boardstuffs
open Pieceobject

class type board_object = 
object
    (**** Get rid of later ****)
    method getsize : int

    method printlistlengths : unit

    method print_rows_elt : index -> unit


    (* Resets the board to be blank *)
    method reset : unit

    (* Returns the list list of indices *)
    method getIndices : index list list

    (* Converts the board index to the universal index *)
    method convertIndex : index -> index

    (* Convers index back *)
    method convertBack : index -> index

    (* Builds an empty board *)
    method buildEmptyBoard : piece_object list list

    (* Builds an empty list of index lists *)
    method buildRows : index list list

	(* Given an index, returns the piece in that location *)
    method getPiece : index -> piece_object option

    (* Inserts pieces on the board *)
    method insert : index -> occupied -> bool

    (* Removes pieces from a board, returns false if the remove is invalid 
       This is only used for testing purposes *)
    method remove : index -> bool

    (* Given an index, returns a list of neighbors *)
    method getNeighbors : index -> (index option)*(index option)

    (* Checks if the board has a winning configuration *)
    method isWin : bool

    (* Given a board, returns all the threats *)
    method getThreats : threat list
end

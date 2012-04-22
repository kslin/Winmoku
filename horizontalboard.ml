open Miniboard
open Boardstuffs
open Piece
open Boardobject

class horizontalboard (size:int): board_object =
object (self)
	inherit miniboard size as super

    method buildEmptyBoard = 
        print_string "\n I am printing from horizontal \n";
        let rec build_board n b = match n with
            |0 -> b
            |_ -> (let rec build_row m r = match m with
                |0 -> r
                |_ -> build_row (m-1) ((new piece Unocc)::r)
                in build_board (n-1) ((build_row size [])::b ) )
        in (build_board size [])

    method buildRows = 
        let rec build_rows n b = match n with
            |0 -> b
            |_ -> (let rec build_row m r = match m with
                |0 -> r
                |_ -> build_row (m-1) ((n-1,m-1)::r)
                in build_rows (n-1) ((build_row size [])::b ) )
        in (build_rows size [])
end
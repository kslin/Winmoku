open Miniboard
open Boardstuffs
open Piece
open Boardobject

class diagleftboard (size:int): board_object =
object (self)
	inherit miniboard size as super

    method buildEmptyBoard = 
        self#buildEmptyDiag size

    method buildRows = 
        let rec emptyDiag (n1: int) (b: index list list)=
            match n1 with
                |0 -> b
                |_ -> let rec emptyList (n2: int) (ind: index list) = 
                    match n2 with
                        |0 -> ind
                        |_ -> emptyList (n2-1) ((n1-1,n2-1)::ind)
                    in emptyDiag (n1-1)            
                        ((emptyList (size - abs(size - n1)) [])::b)
        in emptyDiag ((2*size) - 1) []

    method copyself = let newboard = (new diagleftboard size) in
        (newboard#setboard (self#getboard);
        newboard#setrows (self#getrows);
        newboard#setblackneighbors (self#getblackneighbors);
        newboard#setwhiteneighbors (self#getwhiteneighbors);
        newboard#setoccrows (self#getoccrows);
        (newboard :> board_object))

    method convertIndex ci = 
        let (x,y) = ci in
        if y > x
        then (size - 1 - (y - x), x)
        else (size - 1 + (x - y), y)

    method convertBack i =
        let (x,y) = i in
        if x < size 
        then (y, size - 1 - x + y)
        else (x + y - size + 1, y) 


    (** Helper Methods **)
    method private buildEmptyList (n1: int) : piece list = 
        let rec emptyList (n: int) (occ: piece list) : piece list = 
            match n with
                |0 -> occ
                |_ -> emptyList (n-1) ((new piece Unocc)::occ)
        in emptyList n1 []

    method private buildEmptyDiag (n1: int) : piece list list =
        let rec emptyDiag (n: int) (b: piece list list) =
            match n with
                |0 -> b
                |_ -> emptyDiag (n-1)            
                    ((self#buildEmptyList (size - abs(size - n)) )::b)
        in emptyDiag ((2*n1) - 1) []

end
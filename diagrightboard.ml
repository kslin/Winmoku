open Miniboard
open Boardstuffs
open Piece
open Boardobject

class diagrightboard (size:int): board_object =
object (self)
	inherit miniboard size as super

    method buildEmptyBoard = 
        let rec emptyDiag (n: int) (b: piece list list) =
            match n with
                |0 -> b
                |_ -> emptyDiag (n-1)            
                    ((self#buildEmptyList (size - abs(size - n)) )::b)
        in emptyDiag ((2*size) - 1) [];

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

    method copyself = let newboard = (new diagrightboard size) in
        (newboard#setboard (self#getboard);
        newboard#setrows (self#getrows);
        newboard#setblackneighbors (self#getblackneighbors);
        newboard#setwhiteneighbors (self#getwhiteneighbors);
        newboard#setoccrows (self#getoccrows);
        (newboard :> board_object))

    method convertIndex i = 
        let (x,y) = i in
        if (x + y) > (size-1) 
        then (x+y, size - y - 1)
        else (x+y, x)

    method convertBack ci = 
        let (x,y) = ci in
        if (x > (size - 1))
        then (x+y + 1 - size, size - y - 1)
        else (y, x - y)

    (** Helper Methods **)
    method private buildEmptyList (len: int) : piece list = 
        let rec emptyList (n: int) (growlist: piece list) : piece list = 
            match n with
                |0 -> growlist
                |_ -> emptyList (n-1) ((new piece Unocc)::growlist)
        in emptyList len []

end
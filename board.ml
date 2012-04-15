(* This file contains all the modules and functions for the Gomoku board *)
exception TODO

open Boardstuffs

let unisize : int = 19

(* This is a signature for a Gomoku board *)
module type BOARD =
sig
    type board

    (* An empty board *)
    val empty: board

    (* Given an index, returns whether it exists and is occupied *)
    val getIndex: board -> index -> occupied option

    (* Inserts pieces on the board *)
    val insert: board -> index -> occupied -> board option

    (* Removes pieces from a board, returns None if the remove is invalid 
       This is only used for testing purposes *)
    val remove: board -> index -> board option

    (* Given an index, returns a list of neighbors *)
    val getNeighbors: board -> index -> index list

    (* Checks if the board has a winning configuration *)
    val isWin: board -> bool

    (* Given a board, returns all the threats *)
    val getThreats: board -> threat list
    
end

(* Argument to be passed into a board *)
module type BOARD_ARG = 
sig
    
    val btype : boardtype
    
    val convertIndex : index -> index

end

(* Argument for the horizontal board *)
module HorizontalBoardArg : BOARD_ARG =
struct

    let btype = Horizontal

    let convertIndex (i: index) = i
end

(* Argument for the vertical board *)
module VerticalBoardArg : BOARD_ARG =
struct

    let btype = Vertical

    (* switch the coordinates *)
    let convertIndex (i: index) = let (x,y) = i in (y,x)
end

(* Argument for the board that goes diagonal right. The first row starts 
   in the top right corner, and rows go from top left to bottom right *)
module DiagRightBoardArg : BOARD_ARG =
struct

    let btype = DiagRight

    let convertIndex (i: index) = 
        let (x,y) = i in 
        if y > x then (unisize - (y-x), x)
        else (unisize + (x-y), y)
end

(* Argument for the board that goes diagonal left. The first row starts 
   in the top left corner, and rows go from bottom left to top right *)
module DiagLeftBoardArg : BOARD_ARG =
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

    type board = occupied list list

    let rec emptyList (n: int) (occ: occupied list) : occupied list = 
            match n with
                |0 -> occ
                |_ -> emptyList (n-1) (Unocc::occ)

    let rec emptySquare (n:int) (b: board) : board =
        match n with
            |0 -> b
            |_ -> emptySquare (n-1) ((emptyList n [])::b)

    let emptyDiag (n: int) (b: board) : board =
        let rec rec_emptyDiag n2 b2 = 
            match n2 with
                |0 -> b
                |_ -> rec_emptyDiag (n2-1)            
                    ((emptyList (n - abs(n - n2)) [])::b2) in
        rec_emptyDiag ((2*n) - 1) b 

    let empty = match B.btype with
        |Horizontal|Vertical -> emptySquare unisize []
        |DiagRight|DiagLeft -> emptyDiag unisize []

    let getIndex (b: board) (i:index) : occupied option = 
        let (x,y) = B.convertIndex i in
            try (Some (List.nth (List.nth b x) y))
                with Failure "nth" -> None

                (*if (x < 1) || (y < 1) then None
                else (match B.btype with 
                    |Horizontal -> 
                        if (x > unisize) || (y > unisize) then None
                        else Some (List.nth (List.nth b x) y)
                    |Vertical -> 
                        if (x > unisize) || (y > unisize) then None
                        else Some (List.nth (List.nth b x) y)
                    |DiagLeft -> 
                        if (x > ((unisize * 2) + 1)) || 
                            (y > (unisize-abs(unisize - x))) then None
                        else Some (List.nth (List.nth b x) y)
                    |DiagRight -> 
                        if (x > ((unisize * 2) + 1)) || 
                            (y > (unisize-abs(unisize - x))) then None
                        else Some (List.nth (List.nth b x) y) )*)

    (* Helper function, index should already be converted *)
    let changeIndex (b: board) (i: index) (c: occupied) : board =
        let (x,y) = i in 
            let rec changerow row yval = match row with
                |[] -> row
                |hd::tl -> if yval = y then c::tl 
                    else hd::(changerow tl (yval+1)) in
        let rec changecol (col: occupied list list) xval = match col with
            |[] -> col   
            |hd::tl -> if xval = x then (changerow hd 1)::tl
                else hd::(changecol tl (xval + 1)) in  
        changecol b 1 


    let insert (b: board) (i: index) (c: occupied) : board option = 
        match getIndex b i with
            |None -> None
            |Some Unocc -> let newi = B.convertIndex i in
                Some (changeIndex b newi c)
            |_ -> None


    let remove (b: board) (i: index) : board option = 
        match getIndex b i with
            |None -> None
            |_ -> let newi = B.convertIndex i in
                   Some (changeIndex b newi Unocc)

    let getNeighbors (b: board) (i: index) : index list = raise TODO

    let isWin (b: board) : bool = raise TODO

    let getThreats (b: board) : threat list = raise TODO
    
end

(*module Board : BOARD =
struct

    type board = 
end*)

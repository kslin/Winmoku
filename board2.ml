exception TODO

open Boardstuffs
open Piece

class type board_object_t = 
object

	(* Initilizes the board *)
	method set_board : piece list list -> unit

	(* Given an index, returns whether it exists and is occupied *)
    method getIndex : index -> occupied option

    (* Inserts pieces on the board *)
    method insert : index -> occupied -> bool

    (* Removes pieces from a board, returns false if the remove is invalid 
       This is only used for testing purposes *)
    method remove : index -> bool

    (* Given an index, returns a list of neighbors *)
    method getNeighbors : index -> index list

    (* Checks if the board has a winning configuration *)
    method isWin : bool

    (* Given a board, returns all the threats *)
    method getThreats : threat list
end

(*let rec emptyList (n: int) (occ: piece list) : piece list = 
    match n with
        |0 -> occ
        |_ -> emptyList (n-1) ((new piece)::occ)

let rec emptySquare (n:int) (b: piece list list) : piece list list =
    match n with
        |0 -> b
        |_ -> emptySquare (n-1) ((emptyList n [])::b)

let emptyDiag (n: int) (b: piece list list) : piece list list =
    let rec rec_emptyDiag n2 b2 = 
        match n2 with
            |0 -> b
            |_ -> rec_emptyDiag (n2-1)            
                ((emptyList (n - abs(n - n2)) [])::b2) in
    rec_emptyDiag ((2*n) - 1) b *)

class miniboard (btype: boardtype) (size: int) : board_object_t =
object (self)


	(* Instance Variables *)

	val mutable board = []

    val mutable neighbor_list : index list list = []

    (* Initializer *)
    method set_board newboard =  board <- (newboard :> (piece list list))

    (* Methods *)

	method getIndex (i:index) : occupied option = 
        let (x,y) = i in
            try (Some ((List.nth (List.nth board x) y)#get_value))
                with Failure "nth"|Invalid_argument "List.nth" -> None

    method private getPiece (i: index) : piece option =
    	let (x,y) = i in
            try (Some ((List.nth (List.nth board x) y)))
                with Failure "nth"|Invalid_argument "List.nth" -> None

    method private convertIndex (i:index) = 
    	match btype with
	    	|Horizontal -> i
    		|Vertical -> let (x,y) = i in (y,x)
    		|DiagRight -> 
    			let (x,y) = i in 
        		if y > x then (size - (y-x), x)
        		else (size + (x-y), y)
        	|DiagLeft -> 
    	    	let (x,y) = i in
        		if (x + y - 1) <= size then (x + y - 1, y)
        		else (x + y - 1, y + 1 - (x + y - size))

    method private changeIndex (i:index) (c:occupied) = 
    	let (x,y) = self#convertIndex i in
    	let rec changerow row yval = match row with
            |[] -> ()
            |hd::tl -> if yval = y then (hd :> piece)#set_value c
                else changerow tl (yval+1) in
        let rec changecol col xval = match col with
            |[] -> ()
            |hd::tl -> if xval = x then (changerow hd 1)
                else changecol tl (xval + 1) in  
        changecol board 1 

    method insert (i:index) (c: occupied) : bool =
    	match self#getIndex i with
    		|None -> false
    		|Some Unocc -> self#changeIndex i c; 
    			(self#getPiece i)#
    			true
    		|_ -> false

    method remove (i: index) : bool =
    	match self#getIndex i with
    		|None -> false
    		|_ -> self#changeIndex i Unocc; true

    method getNeighbors (i:index) = 
    	let (x,y) = i in
    	match self#getIndex i with
    		|None -> []
    		|Some mycolor ->
    	(match (self#getIndex (x,y-1)), (self#getIndex (x,y+1)) with
    		|(None,None) -> []
    		|(None, Some n2) -> if n2 = mycolor then (x,y+1)::[] else []
    		|(Some n1, None) -> if n1 = mycolor then (x,y-1)::[] else []
    		|(Some n1, Some n2) -> (match ((n1 = mycolor), n2 = mycolor) with
    			|(false,false) -> []
    			|(false, true) -> (x,y+1)::[]
    			|(true, false) -> (x,y-1)::[]
    			|_ -> (x,y-1)::((x,y+1)::[])  ) )



    method isWin : bool = 
    	let rec checkNeighbors lst = match lst with
    		|[] -> false
    		|hd::tl -> if List.length hd > 4 then true
    			else checkNeighbors tl in
    	checkNeighbors neighbor_list


    method getThreats = raise TODO



end

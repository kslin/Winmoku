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

	method private getIndex (i:index) : occupied option = 
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
        		else (x + y - 1, 1 - x  + size)

    method private convertBack (ci:index) = 
    	match btype with
	    	|Horizontal -> ci
    		|Vertical -> let (x,y) = ci in (y,x)
    		|DiagRight -> 
    			let (x,y) = ci in
    			if x < size then (y, (size-x)+y)
    			else (x+y-size, y) 
        	|DiagLeft -> 
        		let (x,y) = ci in
        		if x <= size then (x-y+1, y)
        		else (1+size-y,x+y-size)

    method private changeIndex (i:index) (c:occupied) = 
    	let (x,y) = i in
    	let rec changerow row yval = match row with
            |[] -> ()
            |hd::tl -> if yval = y then (hd :> piece)#set_value c
                else changerow tl (yval+1) in
        let rec changecol col xval = match col with
            |[] -> ()
            |hd::tl -> if xval = x then (changerow hd 1)
                else changecol tl (xval + 1) in  
        changecol board 1 

    (* Given an index and a color, insert the piece and update neighbors *)
    method insert (ci:index) (c: occupied) : bool =
    	let i = self#convertIndex ci in
    	match self#getIndex i with
    		|None -> false
    		|Some Unocc -> self#changeIndex i c; 
    			List.iter (fun x -> self#addNeighbors x i) 
    			            (self#getNeighbors i);  
    			true
    		|_ -> false

    method remove (ci: index) : bool =
    	let i = self#convertIndex ci in
    	match self#getIndex i with
    		|None -> false
    		|_ -> self#changeIndex i Unocc; true

    method private getNeighbors (i:index) = 
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


    method getThreats =  raise TODO


    (*method private hasNeighbors (i:index) : index list option=  
    	try (Some (List.find (List.mem i) neighbor_list))
    	with Not_found -> None*)

    (* If n1 is already part of a neighbor list, add n2 to the list.
       If not, make the two into a new neighbor list *)
    method private addNeighbors (i1: index) (i2:index) =
    	let n1 = self#convertBack i1 in
    	let n2 = self#convertBack i2 in
    	let rec findneighlist lst =
    		match lst with 
    			|[] -> (n1::n2::[])::neighbor_list
    			|hd::tl -> if List.mem n1 hd then (n2::hd)::tl
    				else hd::(findneighlist tl)
    	in neighbor_list <- (findneighlist neighbor_list)


end

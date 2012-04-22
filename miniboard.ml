exception ERROR

open Boardstuffs
open Boardobject
open Pieceobject


class miniboard (size: int) : board_object =
object (self)

	(* Instance Variables *)

	val mutable board : piece_object list list = [[]]

    val mutable rows = []

    (* Stores the lists of neighbors *)
    val mutable neighbor_list : index list list = []

    (* Stores the rows that have pieces occupying it *)
    val mutable occ_rows : int list = []

    (* Methods *)

    method getsize = (List.length board)

    method printlistlengths = 
        let rec rec_print n = match n with
            |0 -> ()
            |x -> print_string "The "; print_int x; print_string "th list in this list has "; 
                print_int (List.length (List.nth board (x-1))); print_string " elements. ";
                print_string "\n";
                rec_print (n-1)
        in rec_print self#getsize;
        print_int self#getsize;
        flush_all ()

    method print_rows_elt (i: index) : unit =
        let (x,y) = self#convertIndex i in
        self#print_tuple (List.nth (List.nth rows x) y) 
                

    method reset = 
        board <- self#buildEmptyBoard;
        rows <- self#buildRows

    method getIndices = rows

    method buildEmptyBoard = []

    method buildRows = []

    method getPiece (ci: index) : piece_object option =
        let (x,y) = ci in
            try (Some ((List.nth (List.nth board x) y)))
                with Failure "nth"|Invalid_argument "List.nth" -> None

	method private getIndex (ci:index) : occupied option = 
        let (x,y) = ci in
            try (Some ((List.nth (List.nth board x) y)#get_value))
                with Failure "nth"|Invalid_argument "List.nth" -> None

    method convertIndex (i:index) = i

    method convertBack (ci:index) = ci

    method private changeIndex (ci:index) (c:occupied) = 
    	let (x,y) = ci in
    	let rec changerow row yval = match row with
            |[] -> ()
            |hd::tl -> if yval = y then (hd :> piece_object)#set_value c
                else changerow tl (yval+1) in
        let rec changecol col xval = match col with
            |[] -> ()
            |hd::tl -> if xval = x then (changerow hd 0)
                else changecol tl (xval + 1) in  
        changecol board 0
        

    method private print_tuple (x,y) = print_string " ("; print_int x; 
        print_string ","; print_int y; print_string ") "; flush_all ()

    (* Given an index and a color, insert the piece and update neighbors *)
    method insert (i:index) (c: occupied) : bool =
    	let ci = self#convertIndex i in
    	let (x,y) = ci in
    	match self#getIndex ci with
    		|None -> false
    		|Some Unocc -> self#changeIndex ci c; 
    			(List.iter (fun a -> self#addNeighbors a ci) 
    			            (self#getNeighbors ci)); 
                (if List.mem x occ_rows then ()
                else occ_rows <- (x::occ_rows) );
                (*print_string "(";
                List.iter (fun x -> print_int x; print_string ", "; flush_all ()) occ_rows;
                print_string ") ";
                flush_all ();*)
                true
    		|_ -> false

    method remove (i: index) : bool =
    	let i = self#convertIndex i in
    	match self#getIndex i with
    		|None -> false
    		|_ -> self#changeIndex i Unocc; true

    method private getNeighbors (ci:index) = 
    	let (x,y) = ci in
    	match self#getIndex ci with
    		|None -> []
            |Some Unocc -> []
    		|Some mycolor ->
    	(match (self#getIndex (x,y-1)), (self#getIndex (x,y+1)) with
    		|(None,None) -> []
    		|(None, Some n2) -> if n2 = mycolor then (x,y+1)::[] else []
    		|(Some n1, None) -> if n1 = mycolor then (x,y-1)::[] else []
    		|(Some n1, Some n2) -> (match ((n1 = mycolor), n2 = mycolor) with
    			|(false,false) -> []
    			|(false, true) -> (x,y+1)::[]
    			|(true, false) ->(x,y-1)::[]
    			|_ -> (x,y-1)::((x,y+1)::[])  ) )



    method isWin : bool = 
    	let rec checkNeighbors lst = match lst with
    		|[] -> false
    		|hd::tl -> if List.length hd > 4 then true
    			else checkNeighbors tl in
    	checkNeighbors neighbor_list


    method getThreats = 
        List.flatten (List.map (fun x -> self#containsThreats (List.nth rows x)) occ_rows)

    (* Determines if a row has threats in it and returns the threats *)
    method private containsThreats (lst: index list) : threat list = 
    	if List.length lst < 5 then []
        else let sixlist = self#hasContSix lst in
    	    (*let rec findfives flst = match flst with
                |[] -> flst
                |hd::tl -> let ((a,aind),(b,bind),
                                (c,cind),(d,dind),
                                (e,eind)) = hd in 
                    (match (aind,bind,cind,dind,eind) with
                        |(Unocc,Black,Black,Unocc,Unocc) -> 
                            (Threat (d,[a,e],[b,c]))::(findfives tl)
                        |(Unocc,Unocc,Black,Black,Unocc) ->
                            (b,[a,e],[c,d])::(findfives tl)
                        |(Unocc,Black,Unocc,Black,Unocc) ->
                            (c,[a,e],[b,d])::(findfives tl) 
                        |_ -> findfives tl ) in *)

            let rec findsixes slst : threat list = match slst with
                |[] -> []
                |hd::tl -> let ((a,aind),(b,bind),
                                (c,cind),(d,dind),
                                (e,eind),(f,find)) = hd in 
                (match (aind,bind,cind,dind,eind,find) with
                    |(Unocc,Black,Black,Black,Unocc,Unocc) -> 
                (Threat (StraightFour,e,[a;f],[b;c;d]))::(findsixes tl) 
                    |(Unocc,Unocc, Black,Black,Black,Unocc) -> 
                (Threat (StraightFour,b,[a;f],[c;d;e]))::(findsixes tl) 
                    |(Unocc,Black,Unocc,Black,Black,Unocc) -> 
                (Threat (StraightFour,c,[a;f],[b;d;e]))::(findsixes tl) 
                    |(Unocc,Black,Black,Unocc,Black,Unocc) -> 
                (Threat (StraightFour,d,[a;f],[b;c;e]))::(findsixes tl)
                    |(Unocc,Black,Black,Unocc,Unocc,Unocc) ->
                (Threat (SplitThree,e,[a;d;f],[b;c]))::(findsixes tl)
                    |(Unocc,Unocc,Black,Unocc,Black,Unocc) ->
                (Threat (SplitThree,b,[a;d;f],[c;e]))::(findsixes tl)
                    |(Unocc,Black,Unocc,Unocc,Black,Unocc) ->
                (Threat (SplitThree,c,[a;d;f],[b;e]))::
                (Threat (SplitThree,d,[a;c;f],[b;e]))::(findsixes tl)
                    |(Unocc,Unocc,Unocc,Black,Black,Unocc) ->
                (Threat (SplitThree,b,[a;c;f],[d;e]))::(findsixes tl)
                    |(Unocc,Black,Unocc,Black,Unocc,Unocc) ->
                (Threat (SplitThree,e,[a;c;f],[b;d]))::(findsixes tl)
                    |_ -> findsixes tl ) in
        (findsixes sixlist) 

                


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

    (*method private addNeighRows (row: index list) = 
    	if List.mem row occ_rows then ()
    	else occ_rows <- (row::occ_rows)*)

    method private sublist lst start endpoint = 
        if endpoint < 4 then None else
    	let listlength = List.length lst in
        if endpoint >= listlength then None else
    	let rec buildlist current lst2 =
    		if current < start then Some lst2
    		else buildlist (current - 1) ((List.nth lst current)::lst2) in
    	buildlist start []


    (* If a row has five continuous spaces that are either occupied by
    	black or unoccupied, return the spaces*)
    method private hasContFive (row: index list) = 
    	if List.length row < 5 then []
    	else let rec iter_row start fivelist = 
    		match self#sublist row start (start + 4) with
    			|None -> fivelist
    			|Some (a::b::c::d::e::[]) -> 
    				let aind = self#getIndex a in
                    let bind = self#getIndex b in
                    let cind = self#getIndex c in
                    let dind = self#getIndex d in
                    let eind = self#getIndex e in
                    if (aind = Some White) ||
                       (bind = Some White) ||
                       (cind = Some White) ||
                       (dind = Some White) ||
                       (eind = Some White)
    				then iter_row (start+1) fivelist
    				else iter_row (start+1) (((a,aind),(b,bind),
                                              (c,cind),(d,dind),
                                              (e,eind))::fivelist)
                |_ -> fivelist in
        iter_row 0 []

    method private deopt x = match x with
        |None -> raise ERROR
        |Some s -> s

    (* If a row has six continuous spaces that are either occupied by
        black or unoccupied, return the spaces*)
    method private hasContSix (row: index list) = 
        if List.length row < 6 then []
        else let rec iter_row start sixlist = 
            match self#sublist row start (start + 5) with
                |None -> sixlist
                |Some (a::b::c::d::e::f::[]) -> 
                    let aind = self#getIndex a in
                    let bind = self#getIndex b in
                    let cind = self#getIndex c in
                    let dind = self#getIndex d in
                    let eind = self#getIndex e in
                    let find = self#getIndex f in
                    if (aind = Some White) ||
                       (bind = Some White) ||
                       (cind = Some White) ||
                       (dind = Some White) ||
                       (eind = Some White) ||
                       (find = Some White)
                    then iter_row (start+1) sixlist
                    else iter_row (start+1) (((a,self#deopt aind),
                                              (b,self#deopt bind),
                                              (c,self#deopt cind),
                                              (d,self#deopt dind),
                                              (e,self#deopt eind),
                                              (f,self#deopt find))::sixlist)
                |_ -> sixlist in
        iter_row 0 []




end

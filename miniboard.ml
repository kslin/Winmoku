exception ERROR

open Boardstuffs
open Boardobject
open Pieceobject


class miniboard (size: int) : board_object =
object (self)

	(* Instance Variables *)

	val mutable board : piece_object list list = [[]]

    val mutable rows : index list list = []

    (* Stores the lists of black neighbors *)
    val mutable black_neighbor_list : index list list = []

    (* Stores the lists of white neighbors *)
    val mutable white_neighbor_list : index list list = []

    (* Stores the rows that have pieces occupying it *)
    val mutable occ_rows : int list = []

    (* Methods *)

    method private copylist (lst: index list) = 
        List.map (fun x -> x) lst

    method private copyintlist (lst: int list) = 
        List.map (fun x -> x) lst        

    method private copylistlist (lst: index list list) = 
        List.map (fun x -> (List.map (fun y -> y) x)) lst

    method getboard = 
        List.map (fun x -> (List.map (fun y -> y#clone) x)) board 

    method setboard b = board <- b

    method getrows = self#copylistlist rows

    method setrows r = rows <- r

    method getblackneighbors = self#copylistlist black_neighbor_list

    method setblackneighbors bn = black_neighbor_list <- bn

    method getwhiteneighbors = self#copylistlist white_neighbor_list

    method setwhiteneighbors wn = white_neighbor_list <- wn

    method getoccrows = self#copyintlist occ_rows

    method setoccrows o = occ_rows <- o

    method getsize = (List.length board)

    method copyself = (new miniboard size)

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
                

    method empty = 
        board <- self#buildEmptyBoard;
        rows <- self#buildRows;
        (self :> board_object)

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
    method insert (i:index) (c: occupied) : board_object option =
        print_int (List.length occ_rows);
        flush_all ();
    	let ci = self#convertIndex i in
    	let (x,y) = ci in
    	match (self#getIndex ci, c) with
    		|(None, _) -> (self#print_tuple i; self#print_tuple ci; flush_all (); None)
    		|(Some Unocc, Black) -> self#changeIndex ci c; 
                (match self#getNeighbors ci with
                    |(None,None) -> 
                        black_neighbor_list <- 
                        [ci]::black_neighbor_list
                    |(Some s, None) ->
                        self#addBlackNeighbors s ci
                    |(None, Some s) ->
                        self#addBlackNeighbors s ci
                    |(Some x, Some y) ->
                        self#addMultBlackNeighbors x ci y );
                (if List.mem x occ_rows then ()
                else occ_rows <- (x::occ_rows) );
                Some (self :> board_object)
            |(Some Unocc, White) -> self#changeIndex ci c; 
                (match self#getNeighbors ci with
                    |(None,None) -> 
                        white_neighbor_list <- 
                        [ci]::white_neighbor_list
                    |(Some s, None) ->
                        self#addWhiteNeighbors s ci
                    |(None, Some s) ->
                        self#addWhiteNeighbors s ci
                    |(Some x, Some y) ->
                        self#addMultWhiteNeighbors x ci y );
                (if List.mem x occ_rows then ()
                else occ_rows <- (x::occ_rows) );
                Some (self :> board_object)
    		|_ -> None

    method remove (i: index) : bool =
    	let i = self#convertIndex i in
    	match self#getIndex i with
    		|None -> false
    		|_ -> self#changeIndex i Unocc; true

    method getNeighbors (ci:index) = 
    	let (x,y) = ci in
    	match self#getIndex ci with
    		|None -> (None, None)
            |Some Unocc -> (None, None)
    		|Some mycolor ->
    	(match (self#getIndex (x,y-1)), (self#getIndex (x,y+1)) with
    		|(None,None) -> (None, None)
    		|(None, Some n2) -> 
                if n2 = mycolor then (Some (x,y+1), None) else (None,None)
    		|(Some n1, None) -> 
                if n1 = mycolor then (Some (x,y-1), None) else (None,None)
    		|(Some n1, Some n2) -> (match ((n1 = mycolor), n2 = mycolor) with
    			|(false,false) -> (None,None)
    			|(false, true) -> (Some (x,y+1), None)
    			|(true, false) -> (Some (x,y-1), None)
    			|_ -> (Some (x,y-1), Some (x,y+1))  ))



    method isWin : bool = 
    	let rec checkNeighbors lst = match lst with
    		|[] -> false
    		|hd::tl -> if List.length hd > 4 then true
    			else checkNeighbors tl in
    	(checkNeighbors black_neighbor_list) || 
        (checkNeighbors white_neighbor_list)


    method getThreats = 
        List.flatten (List.map (fun x -> self#containsThreats (List.nth rows x)) occ_rows)

    method private containsThreats (lst: index list) : threat list =
        if List.length lst < 5 then []
        else (let rec rec_findthreats threats lst = match lst with
            |[] -> threats
            |hd::tl -> 
                if List.length hd = 3 
                then rec_findthreats ((self#handle_threes hd)@threats) tl
                else if List.length hd = 2
                then rec_findthreats ((self#handle_twos hd)@threats) tl
                else if List.length hd = 1
                then rec_findthreats ((self#handle_ones hd)@threats) tl
                else [] 
            in rec_findthreats [] black_neighbor_list )

    method private handle_threes (threes: index list) : threat list =
        match threes with
            |(x1,y1)::(x2,y2)::(x3,y3)::[] -> let (a,b,c,d) = 
                    (self#convertBack (x1,y1-2),self#convertBack (x1,y1-1),
                    self#convertBack (x3,y3+1),self#convertBack (x3,y3+2)) in
                let lst = [self#convertBack (x1,y1);self#convertBack (x2,y2);
                           self#convertBack (x3,y3)] in
                (match (self#getIndex (x1,y1-2), self#getIndex b,
                self#getIndex c, self#getIndex d) with
                    |(_, (None|Some White), Some Unocc, Some Unocc) ->
                        [Threat(Four, c, [d], lst);
                         Threat(Four, d, [c], lst)]
                    |((None|Some White), Some Unocc, Some Unocc, (None|Some White)) ->
                        [Threat(Four, b, [c], lst);
                         Threat(Four, c, [b], lst)]
                    |((None|Some White), Some Unocc, Some Unocc, Some Unocc) ->
                        [Threat(StraightFour, c, [b;d], lst)]
                    |(Some Unocc, Some Unocc, (None|Some White), _) ->
                        [Threat(Four, b, [a], lst);
                         Threat(Four, a, [b], lst)]
                    |(Some Unocc, Some Unocc, Some Unocc, (None|Some White)) ->
                        [Threat(StraightFour, b, [a;c], lst)]
                    |(Some Unocc, Some Unocc, Some Unocc, Some Unocc) ->
                        [Threat(StraightFour, b, [a;c], lst);
                         Threat(StraightFour, c, [b;d], lst)]
                    |_ -> [])
            |_ -> raise ERROR

    method private handle_twos (twos: index list) : threat list = 
        match twos with
            |(x1,y1)::(x2,y2)::[] -> let (a,b,c,d,e,f) = 
                        (self#convertBack (x1,y1-3), self#convertBack (x1,y1-2), 
                         self#convertBack (x1,y1-1), self#convertBack (x2,y2+1), 
                         self#convertBack (x2,y2+2), self#convertBack (x2,y2+3)) in
                let lst = [self#convertBack (x1,y1); self#convertBack (x2,y2)] in
                (match (self#getIndex (x1,y1-3), self#getIndex (x1,y1-2),
                        self#getIndex (x1,y1-1), self#getIndex (x2,y2+1),
                        self#getIndex (x2,y2+2), self#getIndex (x2,y2+3)) with
                    |(_,_,(None|Some White),
                     Some Unocc, Some Black, Some Unocc) ->
                        [Threat(Four, d, [f], lst@[e]);
                         Threat(Four, f, [d], lst@[e])]
                    |(Some Unocc, Some Black, Some Unocc,
                     (None|Some White),_,_) ->
                        [Threat(Four, c, [a], b::lst);
                         Threat(Four, a, [c], b::lst)]
                    |((None|Some White), Some Black, Some Unocc,
                     Some Unocc, _, _) -> 
                        [Threat(Four, c, [d], b::lst);
                         Threat(Four, d, [c], b::lst)]
                    |(_,_,Some Unocc,
                     Some Unocc, Some Black, (None|Some White)) ->
                        [Threat(Four, c, [d], lst@[e]);
                         Threat(Four, d, [c], lst@[e])]
                    |(_,_,Some Unocc,
                     Some Unocc, Some Black, Some Unocc) ->
                        [Threat(StraightFour, d, [c;f], lst@[e])]
                    |(Some Unocc, Some Black, Some Unocc,
                     Some Unocc, _, _) ->
                        [Threat(StraightFour, c, [a;d], b::lst)]

                    |(_, (None|Some White), Some Unocc,
                     Some Unocc, Some Unocc, (None|Some White)) ->
                        [Threat(WallThree, d, [c;f], lst)]
                    |((None|Some White), Some Unocc, Some Unocc,
                     Some Unocc, (None|Some White), _ ) ->
                        [Threat(WallThree, c, [b;d], lst)]
                    |(_,(None|Some White), Some Unocc,
                     Some Unocc, Some Unocc, Some Unocc) ->
                        [Threat(WallThree, d, [c;e;f], lst);
                         Threat(SplitThree, e, [c;d;f], lst)]
                    |(Some Unocc, Some Unocc, Some Unocc,
                     Some Unocc, (None|Some White), _) ->
                        [Threat(WallThree, c, [a;b;d], lst);
                         Threat(SplitThree, b, [a;c;d], lst)]
                    |((None|Some White), Some Unocc, Some Unocc,
                     Some Unocc, Some Unocc, (None|Some White)) ->
                        [Threat(WallThree, c, [b;d;e], lst);
                         Threat(WallThree, d, [b;c;e], lst)]
                    |((None|Some White), Some Unocc, Some Unocc,
                     Some Unocc, Some Unocc, Some Unocc) ->
                        [Threat(WallThree, c, [b;d;e],lst);
                         Threat(Three, d, [c;e],lst);
                         Threat(SplitThree, e, [c;d;f],lst)]
                    |(Some Unocc, Some Unocc, Some Unocc,
                     Some Unocc, Some Unocc, (None|Some White)) ->
                        [Threat(WallThree, d, [b;c;e],lst);
                         Threat(Three, c, [b;d],lst);
                         Threat(SplitThree, b, [a;c;d],lst)]
                    |(Some Unocc, Some Unocc, Some Unocc,
                     Some Unocc, Some Unocc, Some Unocc) ->
                        [Threat(Three, c, [b;d],lst);
                         Threat(Three, d, [c;e],lst);
                         Threat(SplitThree, b, [a;c;d], lst);
                         Threat(SplitThree, e, [c;d;f], lst)]
                    |_ -> [])
            |_ -> raise ERROR 

    method private handle_ones (lst: index list) : threat list = 
        match lst with
            |(x,y)::[] -> let (a,b,c,d,e,f) = 
                        (self#convertBack (x,y-2),self#convertBack (x,y-1),
                         self#convertBack (x,y+1),self#convertBack (x,y+2),
                         self#convertBack (x,y+3),self#convertBack (x,y+4)) in
                let lst = [self#convertBack (x,y)] in
                (match (self#getIndex (x,y-2), self#getIndex (x,y-1),
                        self#getIndex (x,y+1), self#getIndex (x,y+2),
                        self#getIndex (x,y+3), self#getIndex (x,y+4)) with
                |((None|Some White), Some Unocc, Some Unocc,
                 Some Black, Some Unocc, Some Unocc) ->
                    [Threat(WallThree, c, [b;e], lst@[d]);
                     Threat(SplitThree, e, [b;c;f], lst@[d])]
                |(Some Unocc, Some Unocc, Some Unocc,
                 Some Black, Some Unocc, (None|Some White)) ->
                    [Threat(WallThree, c, [b;d], lst@[d]);
                     Threat(SplitThree, b, [a;c;f], lst@[d]);]
                |(Some Unocc, Some Unocc, Some Unocc,
                 Some Black, Some Unocc, Some Unocc) ->
                    [Threat(Three, c, [b;e], lst);
                     Threat(SplitThree, b, [a;c;e], lst@[d]);
                     Threat(SplitThree, e, [b;c;f], lst@[d])]
                |_ -> [] )
            |_ -> raise ERROR

                

    (* If i1 is already part of a neighbor list, add i2 to the list.
       If not, make the two into a new neighbor list *)
    method private addBlackNeighbors (ci1: index) (ci2:index) =
    	let rec findneighlist lst =
    		match lst with 
    			|[] -> (self#tuple_sort(ci1::ci2::[]))::black_neighbor_list
    			|hd::tl -> 
                    if List.mem ci1 hd then (self#tuple_sort(ci2::hd))::tl
    				else hd::(findneighlist tl)
    	in black_neighbor_list <- (findneighlist black_neighbor_list)

    method private addWhiteNeighbors (ci1: index) (ci2:index) =
        let rec findneighlist lst =
            match lst with 
                |[] -> (self#tuple_sort(ci1::ci2::[]))::white_neighbor_list
                |hd::tl -> 
                    if List.mem ci1 hd then (self#tuple_sort(ci2::hd))::tl
                    else hd::(findneighlist tl)
        in white_neighbor_list <- (findneighlist white_neighbor_list)

    (* If i1 or i2 are already in a neighbor list, add the other 2 to 
        the same list. If both are in a list, merge the lists. If 
        none are in a list, make a new neighbor list *)
    method private addMultBlackNeighbors (ci1: index) (ci2: index) (ci3: index) =
        let rec findneighlist lst isfirst isthird rest =
            match lst with
                |[] -> (isfirst, isthird, rest)
                |hd::tl -> (match (List.mem ci1 hd, List.mem ci3 hd) with
                    |(false, false) -> 
                        findneighlist tl isfirst isthird (hd::rest)
                    |(true, false) -> 
                        findneighlist tl hd isthird rest
                    |(false, true) -> 
                        findneighlist tl isfirst hd rest
                    |(true, true) -> 
                        raise ERROR)
        in match findneighlist black_neighbor_list [] [] [] with
            |([], [], r) -> 
                black_neighbor_list <- 
                    (self#tuple_sort (ci1::ci2::ci3::[]))::black_neighbor_list
            |(x, [], r) -> 
                black_neighbor_list <- (self#tuple_sort(ci2::ci3::x))::r
            |([], y, r) -> 
                black_neighbor_list <- (self#tuple_sort(ci1::ci2::y))::r
            |(x, y, r) -> 
                black_neighbor_list <- (self#tuple_sort(ci2::(x@y)))::r

    method private addMultWhiteNeighbors (ci1: index) (ci2: index) (ci3: index) =
        let rec findneighlist lst isfirst isthird rest =
            match lst with
                |[] -> (isfirst, isthird, rest)
                |hd::tl -> (match (List.mem ci1 hd, List.mem ci3 hd) with
                    |(false, false) -> 
                        findneighlist tl isfirst isthird (hd::rest)
                    |(true, false) -> 
                        findneighlist tl hd isthird rest
                    |(false, true) -> 
                        findneighlist tl isfirst hd rest
                    |(true, true) -> 
                        raise ERROR)
        in match findneighlist white_neighbor_list [] [] [] with
            |([], [], r) -> 
                white_neighbor_list <- 
                    (self#tuple_sort (ci1::ci2::ci3::[]))::white_neighbor_list
            |(x, [], r) -> 
                white_neighbor_list <- (self#tuple_sort(ci2::ci3::x))::r
            |([], y, r) -> 
                white_neighbor_list <- (self#tuple_sort(ci1::ci2::y))::r
            |(x, y, r) -> 
                white_neighbor_list <- (self#tuple_sort(ci2::(x@y)))::r

    method private tuple_sort lst = 
        List.sort (fun (x1,y1) (x2,y2) -> y1 - y2) lst 


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

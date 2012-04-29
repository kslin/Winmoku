exception ERROR

open Boardstuffs

(* Module for representing one component of the whole board
    For example, the horizontal component *)
module type BOARDCOMP =
sig 
    (* Defines the type boardcomp *)
	type boardcomp

    (* Returns an empty board*)
	val empty : boardcomp

    (* Given an board and index, returns the color of that index *)
    val getIndex : boardcomp -> index -> occupied option

    (* Given a board, return its representation of the indices*)
	val getIndices : boardcomp -> index list list

    (* Given a universal index, converts to the local index *)
	val convertIndex : index -> index 

    (* Converts a local index back into a universal index *)
	val convertBack : index -> index 

    (* Gets the pieces of a board *)
    val getPieceArray : boardcomp -> occupied list list

    (* Insert a piece into the board *)
	val insert : boardcomp -> index -> occupied -> boardcomp option

    (* Determines if the board has a winning configuration *)
	val isWin : boardcomp -> occupied option

    (* Returns all the threats on the board *)
	val getThreats : boardcomp -> threat list

    (* Determines if there can be a win the next turn *)
    val nextWin : boardcomp -> index option

    (* Returns a score regarding the number of groups of black 
        and white pieces *)
    val getNeighbors : boardcomp -> int*int

end

(* Arguments to construct a BOARDCOMP *)
module type BOARD_ARG =
sig
    (* Builds an empty board of pieces *)
	val buildEmptyBoard : unit -> occupied list list

    (* Builds an empty list list of indices *)
	val buildRows : unit -> index list list

    (* Function for converting a universal index to a local one *)
	val convert : index -> index

    (* Function for converting a local index back to a universal one *)
	val revert : index -> index
end

(* Arg for a horizontal BOARDCOMP *)
module HorizontalBoardArg : BOARD_ARG =
struct

	let buildEmptyBoard () = 
        let rec build_board n b = match n with
            |0 -> b
            |_ -> (let rec build_row m r = match m with
                |0 -> r
                |_ -> build_row (m-1) (Unocc::r)
                in build_board (n-1) ((build_row world_size [])::b ) )
        in (build_board world_size [])

    let buildRows () = 
        let rec build_rows n b = match n with
            |0 -> b
            |_ -> (let rec build_row m r = match m with
                |0 -> r
                |_ -> build_row (m-1) ((n-1,m-1)::r)
                in build_rows (n-1) ((build_row world_size [])::b ) )
        in (build_rows world_size [])

    let convert i = let (x,y) = i in (y,x)

    let revert ci = let (x,y) = ci in (y,x)

end

(* Arg for a vertical BOARDCOMP *)
module VerticalBoardArg : BOARD_ARG =
struct
	let buildEmptyBoard () = 
        let rec build_board n b = match n with
            |0 -> b
            |_ -> (let rec build_row m r = match m with
                |0 -> r
                |_ -> build_row (m-1) (Unocc::r)
                in build_board (n-1) ((build_row world_size [])::b ) )
        in (build_board world_size [])

    let buildRows () = 
        let rec build_rows n b = match n with
            |0 -> b
            |_ -> (let rec build_row m r = match m with
                |0 -> r
                |_ -> build_row (m-1) ((n-1,m-1)::r)
                in build_rows (n-1) ((build_row world_size [])::b ) )
        in (build_rows world_size [])

    let convert i =  i

    let revert ci = ci

end

(* Arg for a diagonal right BOARDCOMP *)
module DiagRightBoardArg : BOARD_ARG =
struct

    let buildEmptyList (len: int) : occupied list = 
        let rec emptyList (n: int) (growlist: occupied list) = 
            match n with
                |0 -> growlist
                |_ -> emptyList (n-1) (Unocc::growlist)
        in emptyList len []

    let buildEmptyBoard () =
        let rec emptyDiag (n: int) (b: occupied list list) =
            match n with
                |0 -> b
                |_ -> emptyDiag (n-1)            
                    ((buildEmptyList (world_size - abs(world_size - n)) )::b)
        in emptyDiag ((2*world_size) - 1) []

    let buildRows () = 
        let rec emptyDiag (n1: int) (b: index list list)=
            match n1 with
                |0 -> b
                |_ -> let rec emptyList (n2: int) (ind: index list) = 
                    (match n2 with
                        |0 -> ind
                        |_ -> emptyList (n2-1) ((n1-1,n2-1)::ind))
                    in emptyDiag (n1-1)            
                    ((emptyList (world_size - abs(world_size - n1)) [])::b)
        in emptyDiag ((2*world_size) - 1) []

    let convert i = 
        let (x,y) = i in
        if (x + y) > (world_size-1) 
        then (x+y, world_size - y - 1)
        else (x+y, x)

    let revert ci = 
        let (x,y) = ci in
        if (x > (world_size - 1))
        then (x+y + 1 - world_size, world_size - y - 1)
        else (y, x - y)

end

(* Arg for a diagonal left BOARDCOMP *)
module DiagLeftBoardArg : BOARD_ARG =
struct

    let buildEmptyList (len: int) : occupied list = 
        let rec emptyList (n: int) (growlist: occupied list) = 
            match n with
                |0 -> growlist
                |_ -> emptyList (n-1) (Unocc::growlist)
        in emptyList len []

    let buildEmptyBoard () = 
        let rec emptyDiag (n: int) (b: occupied list list) =
            match n with
                |0 -> b
                |_ -> emptyDiag (n-1)            
                ((buildEmptyList (world_size - abs(world_size - n)) )::b)
        in emptyDiag ((2*world_size) - 1) []

    let buildRows () = 
        let rec emptyDiag (n1: int) (b: index list list)=
            match n1 with
                |0 -> b
                |_ -> let rec emptyList (n2: int) (ind: index list) = 
                    match n2 with
                        |0 -> ind
                        |_ -> emptyList (n2-1) ((n1-1,n2-1)::ind)
                    in emptyDiag (n1-1)            
                    ((emptyList (world_size - abs(world_size - n1)) [])::b)
        in emptyDiag ((2*world_size) - 1) []

    let convert i = 
        let (x,y) = i in
        if y > x
        then (world_size - 1 - (y - x), x)
        else (world_size - 1 + (x - y), y)

    let revert ci =
        let (x,y) = ci in
        if x < world_size 
        then (y, world_size - 1 - x + y)
        else (x + y - world_size + 1, y) 

end

(* Functor for making the different kinds of boardcomps *)
module BoardComp (B:BOARD_ARG) : BOARDCOMP =
struct

    type boardcomp = (occupied list list)*(index list list)*
                    (index list list)*(index list list)

    let empty : boardcomp = (B.buildEmptyBoard () , B.buildRows (),[],[]) 

    let getIndices (b:boardcomp) = 
        let (_,rows,_,_) = b in rows

    let convertIndex i = B.convert i

    let convertBack ci = B.revert ci

    let getPieceArray (b:boardcomp) = let (pa,_,_,_) = b in pa

    let getIndex (b:boardcomp) (ci:index) : occupied option = 
        let (pieces,_,_,_) = b in
        let (x,y) = ci in
            try (Some ((List.nth (List.nth pieces x) y)))
                with Failure "nth"|Invalid_argument "List.nth" -> None

    (* Helper function for insertPieces *)
    let insertIntoRow (lst: occupied list) (r: int) (c:occupied) =
        let rec rec_insert row col =
            match row with
                |[] -> []
                |hd::tl -> if col = r then c::tl
                    else hd::(rec_insert tl (col+1))
        in rec_insert lst 0

    (***********************************)
    (*** Helper functions for insert ***)
    (***********************************)

    (* Inserts a specified piece color into an occupied list list *)
    let insertPieces (lst: occupied list list) (ci:index) (c:occupied) =
        let (x,y) = ci in 
        let rec insertRows rows row =
            match rows with 
                |[] -> []
                |hd::tl -> if row = x then (insertIntoRow hd y c)::tl
                    else hd::(insertRows tl (row+1))
        in insertRows lst 0

    (* Determine if pieces next to a space are the same color
       Returns neighbors of the same color *)
    let getNeighbors (b:boardcomp) (ci:index) (c:occupied): 
        (index option)*(index option) = 
        let (x,y) = ci in
        match c with
            |Unocc -> (None, None)
            |mycolor ->
        (match (getIndex b (x,y-1)), (getIndex b (x,y+1)) with
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

    (* If ci1 is already part of a neighbor list, add ci2 to that list
       Otherwise, make both into a new neighbor list *)
    let addNeighbors (neighlist: index list list) (ci1: index) (ci2:index) =   
        let rec findneighlist lst =
            match lst with 
                |[] -> (tuple_sort(ci1::ci2::[]))::neighlist
                |hd::tl -> 
                    if List.mem ci1 hd then (tuple_sort(ci2::hd))::tl
                    else hd::(findneighlist tl)
        in findneighlist neighlist

    (* If ci1 or ci3 are part of neighbor lists, add the other to that list
       If both are in a neighbor list, merge the two lists
       If neither are, make a new neighbor list *)
    let addMultNeighbors (neighlist: index list list) 
                    (ci1: index) (ci2: index) (ci3: index) =
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
        in match findneighlist neighlist [] [] [] with
            |([], [], r) -> 
                (tuple_sort(ci1::ci2::ci3::[]))::neighlist
            |(x, [], r) -> 
                (tuple_sort(ci2::ci3::x))::r
            |([], y, r) -> 
                (tuple_sort(ci1::ci2::y))::r
            |(x, y, r) -> 
                (tuple_sort(ci2::(x@y)))::r

    (*********************************)
    (*********************************)

    let insert (b:boardcomp) (i:index) (c: occupied) : boardcomp option =
        let (pieces,rows,bn,wn) = b in 
        let ci = convertIndex i in
        match (getIndex b ci, c) with
            |(None, _) -> (None)
            |(Some Unocc, Black) -> 
                let newpieces = insertPieces pieces ci c in 
                (match (getNeighbors b ci c) with
                    |(None,None) -> 
                        Some (newpieces,rows,([ci]::bn),wn) 
                    |(Some s, None)|(None, Some s) ->
                        let newbn = addNeighbors bn s ci in
                        Some (newpieces,rows,newbn,wn)
                    |(Some n1, Some n2) ->
                        let newbn = addMultNeighbors bn n1 ci n2 in
                        Some (newpieces,rows,newbn,wn)
                )
            |(Some Unocc, White) -> 
                let newpieces = insertPieces pieces ci c in 
                (match (getNeighbors b ci c) with
                    |(None,None) -> 
                        Some (newpieces,rows,bn,([ci]::wn))
                    |(Some s, None)|(None, Some s) ->
                        let newwn = addNeighbors wn s ci in
                        Some (newpieces,rows,bn,newwn)
                    |(Some n1, Some n2) ->
                        let newwn = addMultNeighbors wn n1 ci n2 in
                        Some (newpieces,rows,bn,newwn)
                )
            |_ -> None

    let isWin (b:boardcomp) : occupied option = 
        let (_,_,bn,wn) = b in
        let rec checkNeighbors lst = match lst with
            |[] -> false
            |hd::tl -> if List.length hd > 4 then true
                else checkNeighbors tl in
        match (checkNeighbors bn, checkNeighbors wn) with
            |(false, false) -> None
            |(true, false) -> Some Black
            |(false, true) -> Some White
            |(true, true) -> raise ERROR

    (* Helper function for getThreats, determins if a three-in-a-row 
        contains threats, returns the threats *)
    let handle_threes (bor:boardcomp) (threes: index list) : threat list =
        match threes with
            |(x1,y1)::(x2,y2)::(x3,y3)::[] -> let (a,b,c,d) = 
                    (convertBack (x1,y1-2),convertBack (x1,y1-1),
                    convertBack (x3,y3+1),convertBack (x3,y3+2)) in
                let lst = [convertBack (x1,y1);convertBack (x2,y2);
                           convertBack (x3,y3)] in
                (match (getIndex bor (x1,y1-2), getIndex bor (x1,y1-1),
                getIndex bor (x3,y3+1), getIndex bor (x3,y3+2)) with
                    |(_, (None|Some White), Some Unocc, Some Unocc) ->
                        [Threat(Four, c, [d], lst);
                         Threat(Four, d, [c], lst)]
                    |((None|Some White), Some Unocc, 
                        Some Unocc, (None|Some White)) ->
                        [Threat(Four, b, [c], lst);
                         Threat(Four, c, [b], lst)]
                    |((None|Some White), Some Unocc, 
                        Some Unocc, Some Unocc) ->
                        [Threat(StraightFour, c, [b;d], lst)]
                    |(Some Unocc, Some Unocc, (None|Some White), _) ->
                        [Threat(Four, b, [a], lst);
                         Threat(Four, a, [b], lst)]
                    |(Some Unocc, Some Unocc, 
                        Some Unocc, (None|Some White)) ->
                        [Threat(StraightFour, b, [a;c], lst)]
                    |(Some Unocc, Some Unocc, Some Unocc, Some Unocc) ->
                        [Threat(StraightFour, b, [a;c], lst);
                         Threat(StraightFour, c, [b;d], lst)]
                    |_ -> [])
            |_ -> raise ERROR

    (* Helper function for getThreats, determins if a twho-in-a-row 
        contains threats, returns the threats *)
    let handle_twos (bor:boardcomp) (twos: index list) : threat list = 
        match twos with
            |(x1,y1)::(x2,y2)::[] -> let (a,b,c,d,e,f) = 
                        (convertBack (x1,y1-3), convertBack (x1,y1-2), 
                         convertBack (x1,y1-1), convertBack (x2,y2+1), 
                         convertBack (x2,y2+2), convertBack (x2,y2+3)) in
                let lst = [convertBack (x1,y1); convertBack (x2,y2)] in
                (match (getIndex bor (x1,y1-3), getIndex bor (x1,y1-2),
                        getIndex bor (x1,y1-1), getIndex bor (x2,y2+1),
                        getIndex bor (x2,y2+2), getIndex bor (x2,y2+3)) with
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
                    |(_,_,_,
                     Some Unocc, Some Unocc, Some Black) ->
                        [Threat(Four, d, [e], lst@[f]);
                         Threat(Four, e, [d], lst@[f])]
                    |(Some Black, Some Unocc, Some Unocc,
                     _,_,_) ->
                        [Threat(Four, b, [c], a::lst);
                         Threat(Four, c, [b], a::lst)]

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

    (* Helper function for getThreats, determins if a singleton 
        contains threats, returns the threats *)
    let handle_ones (bor:boardcomp) (lst: index list) : threat list = 
        match lst with
            |(x,y)::[] -> let (a,b,c,d,e,f) = 
                        (convertBack (x,y-2),convertBack (x,y-1),
                         convertBack (x,y+1),convertBack (x,y+2),
                         convertBack (x,y+3),convertBack (x,y+4)) in
                let lst = [convertBack (x,y)] in
                (match (getIndex bor (x,y-2), getIndex bor (x,y-1),
                        getIndex bor (x,y+1), getIndex bor (x,y+2),
                        getIndex bor (x,y+3), getIndex bor (x,y+4)) with
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
                |(Some Unocc,Some Unocc,Some Unocc,
                 Some Black, Some Unocc, Some Black) ->
                    [Threat(Four, c, [e], lst@[d;f]);
                     Threat(Four, e, [c], lst@[d;f])]
                |_ -> [] )
            |_ -> raise ERROR

    let getThreats (bor:boardcomp): threat list =
        let (_,_,bn,_) = bor in
        let rec rec_findthreats threats lst = match lst with
            |[] -> threats
            |hd::tl -> 
                if List.length hd = 3 
                then rec_findthreats ((handle_threes bor hd)@threats) tl
                else if List.length hd = 2
                then rec_findthreats ((handle_twos bor hd)@threats) tl
                else if List.length hd = 1
                then rec_findthreats ((handle_ones bor hd)@threats) tl
                else rec_findthreats threats tl 
            in rec_findthreats [] bn

    (* Helper function for nextWin, checks if a four can be a win *)
    let checkFour (b:boardcomp) (lst:index list) : index option =
        match lst with
            |[(x1,y1);n2;n3;(x4,y4)] -> 
                if getIndex b (x1,y1-1) = Some Unocc
                then Some (convertBack (x1,y1-1))
                else if getIndex b (x4,y4+1) = Some Unocc
                then Some (convertBack (x4,y4+1))
                else None
            |_ -> raise ERROR

    (* Helper function for nextWin, checks if a three can be a win *)
    let checkThree (b:boardcomp) (lst:index list) (c:occupied): index option =
        match c with
            |Black -> (match lst with
                |[(x1,y1);n2;(x3,y3)] -> 
                    (match (getIndex b (x1, y1-2), getIndex b (x1,y1-1)) with
                        |(Some Black, Some Unocc) -> 
                            Some (convertBack (x1,y1-1))
                        |_ -> 
                            (match (getIndex b (x3,y3+1), 
                                    getIndex b (x3, y3+2)) with
                                |(Some Unocc, Some Black) -> 
                                    Some (convertBack (x3,y3+1))
                                |_ -> None))
                |_ -> raise ERROR)
            |White -> (match lst with
                |[(x1,y1);n2;(x3,y3)] -> 
                    (match (getIndex b (x1, y1-2), 
                            getIndex b (x1,y1-1)) with
                        |(Some White, Some Unocc) -> 
                            Some (convertBack (x1,y1-1))
                        |_ -> 
                            (match (getIndex b (x3,y3+1), 
                                    getIndex b (x3, y3+2)) with
                                |(Some Unocc, Some White) -> 
                                    Some (convertBack (x3,y3+1))
                                |_ -> None))
                |_ -> raise ERROR)
            |_ -> raise ERROR

    (* Helper function for nextWin, checks if a two can be a win *)
    let checkTwo (b:boardcomp) (lst:index list) (c:occupied) : index option =
        match c with
            |Black -> (match lst with
                |[(x1,y1);(x2,y2)] -> 
                    (match (getIndex b (x1, y1-3), 
                            getIndex b (x1,y1-2),
                            getIndex b (x1,y1-1)) with
                        |(Some Black, Some Black, Some Unocc) -> 
                            Some (convertBack (x1,y1-1))
                        |_ -> 
                            (match (getIndex b (x2,y2+1), 
                                    getIndex b (x2, y2+2), 
                                    getIndex b (x2, y2+3)) with
                                |(Some Unocc, Some Black, Some Black) -> 
                                    Some (convertBack (x2,y2+1))
                                |_ -> None))
                |_ -> raise ERROR)
            |White -> (match lst with
                |[(x1,y1);(x2,y2)] -> 
                    (match (getIndex b (x1,y1-3), 
                            getIndex b (x1,y1-2),
                            getIndex b (x1,y1-1)) with
                        |(Some White, Some White, Some Unocc) -> 
                            Some (convertBack (x1,y1-1))
                        |_ -> 
                            (match (getIndex b (x2,y2+1), 
                                    getIndex b (x2,y2+2), 
                                    getIndex b (x2,y2+3)) with
                                |(Some Unocc, Some White, Some White) -> 
                                    Some (convertBack (x2,y2+1))
                                |_ -> None))
                |_ -> raise ERROR)
            |_ -> raise ERROR

    let nextWin (b:boardcomp) : index option = 
        let (_,_,bn,wn) = b in
        let rec checkNeighbors lst c = match lst with
            |[] -> None
            |hd::tl -> match List.length hd with 
                |4 -> (match checkFour b hd with
                    |None -> checkNeighbors tl c
                    |Some s -> Some s)
                |3 -> (match checkThree b hd c with
                    |None -> checkNeighbors tl c
                    |Some s -> Some s)
                |2 -> (match checkTwo b hd c with
                    |None -> checkNeighbors tl c
                    |Some s -> Some s)
                |_ -> checkNeighbors tl c
        in checkNeighbors bn Black 

    let getNeighbors (b:boardcomp) : int*int =
        let (_,_,bn,wn) = b in
        let rec addNeighbors lst score = match lst with
            |[] -> score
            |hd::tl -> addNeighbors tl (score + (List.length hd) - 1)
        in (addNeighbors bn 0, addNeighbors wn 0)

    

end







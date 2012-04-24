exception ERROR

open Boardstuffs
open Pieceobject
open Piece

module type BOARDCOMP =
sig 
	type boardcomp

	val empty : boardcomp

	val getIndices : boardcomp -> index list list

	val convertIndex : index -> index 

	val convertBack : index -> index 

	val insert : boardcomp -> index -> occupied -> boardcomp option

	val isWin : boardcomp -> bool

	val getThreats : boardcomp -> threat list
end

module type BOARD_ARG =
sig
	val buildEmptyBoard : unit -> occupied list list

	val buildRows : unit -> index list list

	val convert : index -> index

	val revert : index -> index
end

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

module DiagRightBoardArg : BOARD_ARG =
struct

    let buildEmptyList (len: int) : piece list = 
        let rec emptyList (n: int) (growlist: piece list) : piece list = 
            match n with
                |0 -> growlist
                |_ -> emptyList (n-1) (Unocc::growlist)
        in emptyList len []

    let buildEmptyBoard () =
        let rec emptyDiag (n: int) (b: piece list list) =
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

module DiagLeftBoardArg : BOARD_ARG =
struct

    let buildEmptyList (len: int) : piece list = 
        let rec emptyList (n: int) (growlist: piece list) : piece list = 
            match n with
                |0 -> growlist
                |_ -> emptyList (n-1) (Unocc::growlist)
        in emptyList len []

    let buildEmptyBoard () = 
        let rec emptyDiag (n: int) (b: piece list list) =
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

module BoardComp (B:BOARD_ARG) : BOARDCOMP =
struct

    type boardcomp = (occupied list list)*(index list list)*
                    (index list list)*(index list list)*(int list)

    let empty : boardcomp = (B.buildEmptyBoard, B.buildRows,[],[],[]) 

    let getIndices (b:boardcomp) = 
        let (_,rows,_,_,_) = b in rows

    let convertIndex i = B.convert i

    let convertBack ci = B.revert ci

    let getIndex (b:boardcomp) (ci:index) : occupied option = 
        let (pieces,_,_,_,_) = b in
        let (x,y) = ci in
            try (Some ((List.nth (List.nth pieces x) y)))
                with Failure "nth"|Invalid_argument "List.nth" -> None

    let insertIntoRow (lst: occupied list) (r: int) (c:occupied) =
        let rec rec_insert row col =
            match row with
                |[] -> []
                |hd::tl -> if col = r then c::tl
                    else hd::(rec_insert tl (col+1) c)
        in rec_insert lst 0

    let insertPieces (lst: occupied list list) (ci:index) (c:occupied) =
        let (x,y) = ci in 
        let rec insertRows rows row =
            match rows with 
                |[] -> []
                |hd::tl -> if row = x then (insertIntoRow hd y c)::tl
                    else hd::(insertRows tl (row+1) c)
        in insertRows lst 0

    let getNeighbors (b:boardcomp) (ci:index) = raise TODO

    let addNeighbors (bn: index list list) (ci1: index) (ci2:index) =
        let rec findneighlist lst =
            match lst with 
                |[] -> (tuple_sort(ci1::ci2::[]))::bn
                |hd::tl -> 
                    if List.mem ci1 hd then (tuple_sort(ci2::hd))::tl
                    else hd::(findneighlist tl)
        in findneighlist bn

    let addMultBlackNeighbors (bn: index list list) 
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

    let insert (b:boardcomp) (i:index) (c: occupied) : board_object option =
        let (pieces,rows,bn,wn,occ) = b in 
        let ci = convertIndex i in
        let (x,y) = ci in
        match (getIndex b ci, c) with
            |(None, _) -> (None)
            |(Some Unocc, Black) -> 
                let newpieces = insertPieces pieces ci c in 
                (match getNeighbors b ci with
                    |(None,None) -> 
                        (if List.mem x occ_rows 
                        then (newpieces,rows,([ci]::bn),wn,occ)
                        else (newpieces,rows,([ci]::bn),wn,(x::occ_rows)) )
                    |(Some s, None)|(None, Some s) ->
                        let newbn = addBlackNeighbors bn s ci in
                        (if List.mem x occ_rows 
                        then (newpieces,rows,newbn,wn,occ)
                        else (newpieces,rows,newbn,wn,(x::occ_rows)) )
                    |(Some n1, Some n2) ->
                        let newbn = addMultBlackNeighbors bn n1 ci b2 in
                        (if List.mem x occ_rows 
                        then (newpieces,rows,newbn,wn,occ)
                        else (newpieces,rows,newbn,wn,(x::occ_rows)) )
            |(Some Unocc, White) -> 
                let newpieces = insertPieces pieces ci c in 
                (match getNeighbors b ci with
                    |(None,None) -> 
                        (if List.mem x occ_rows 
                        then (newpieces,rows,([ci]::bn),wn,occ)
                        else (newpieces,rows,([ci]::bn),wn,(x::occ_rows)) )
                    |(Some s, None)|(None, Some s) ->
                        let newwn = addWhiteNeighbors wn s ci in
                        (if List.mem x occ_rows 
                        then (newpieces,rows,bn,newwn,occ)
                        else (newpieces,rows,bn,newwn,(x::occ_rows)) )
                    |(Some n1, Some n2) ->
                        let newwn = addMultWhiteNeighbors wn n1 ci b2 in
                        (if List.mem x occ_rows 
                        then (newpieces,rows,bn,newwn,occ)
                        else (newpieces,rows,bn,newwn,(x::occ_rows)) )
            |_ -> None

    let isWin : boardcomp -> bool

    let getThreats : boardcomp -> threat list

end







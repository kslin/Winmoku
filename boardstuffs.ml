(*module IntMap = Map.Make(struct type t = int let compare = compare end)*)

type boardtype = Horizontal | Vertical | DiagLeft | DiagRight 

type index = int*int 

(* A threat has a gain square, cost squares, and rest squares *)
type threat = Threat of index * index list * index list 

type occupied = Black | White | Unocc 

type boardstruct = occupied list list

(*let rec emptyList (n: int) (occ: occupied list) : occupied list = 
	match n with
		|0 -> occ
		|_ -> emptyList (n-1) (Unocc::occ) ;;

let rec emptySquare (n:int) (b: boardstruct) : boardstruct =
	match n with
        |0 -> b
        |_ -> emptySquare (n-1) ((emptyList n [])::b)

let emptyDiag (n: int) (b: boardstruct) : boardstruct =
	let rec rec_emptyDiag n2 b2 = 
		match n2 with
        	|0 -> b
        	|_ -> rec_emptyDiag (n2-1)            
        		((emptyList (n - abs(n - n2)) [])::b2) in
    rec_emptyDiag ((2*n) - 1) b *)

(*let rec emptyList (n: int) (occ: occupied list) : occupied list = 
            match n with
                |0 -> occ
                |_ -> emptyList (n-1) (Unocc::occ)

    let rec emptySquare (n:int) (b: occupied list list) : occupied list list =
        match n with
            |0 -> b
            |_ -> emptySquare (n-1) ((emptyList n [])::b)

    let emptyDiag (n: int) (b: occupied list list) : occupied list list =
        let rec rec_emptyDiag n2 b2 = 
            match n2 with
                |0 -> b
                |_ -> rec_emptyDiag (n2-1)            
                    ((emptyList (n - abs(n - n2)) [])::b2) in
        rec_emptyDiag ((2*n) - 1) b *)
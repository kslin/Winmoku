exception Error

open Event
open Miniboard
open Pieceobject
open Boardstuffs
open Boardobject
open Horizontalboard
open Verticalboard
open Diagrightboard
open Diagleftboard
open Piece

module type BOARD =
sig 
	type board

	val empty : board

	val get : board -> index -> piece_object

	val getColor : board -> occupied

  val printcolor : board -> unit

	val insert : board -> index -> board

	val insertspecial : board -> index -> occupied -> board

	val isWin : board -> bool

	val getThreats : board -> threat list

	val indices : board -> (index -> unit) -> unit
end

module Myboard : BOARD =
struct

	(**Board will be an array of pieces, 4 miniboards, and who's turn it is **)
	type board = occupied*(piece_object array array)*(board_object)*
                        (board_object)*(board_object)*(board_object)

  	(** Represent the board as an array of pieces**)
  	(*let board : piece_object array array = 
  		Array.make_matrix world_size world_size ((new piece Unocc))

  	(** The board is also represented by 4 miniboards **)
  	let horboard : board_object ref = ref (new horizontalboard world_size)
  	let verboard : board_object ref = ref (new verticalboard world_size)
  	let dgrboard : board_object ref = ref (new diagrightboard world_size)
  	let dglboard : board_object ref = ref (new diagleftboard world_size)

  	(** Keeps track of who's turn it is **)
  	let player : occupied ref = ref Black*)

  	(** Board Operations **)

  	(** Reset to a blank board *)
  	let empty : board = 
  		let piecearray = 
  			Array.make_matrix world_size world_size (new piece Unocc) in
  		let horboard = (new horizontalboard world_size)#empty in
  		let verboard = (new verticalboard world_size)#empty in
  		let dgrboard = (new diagrightboard world_size)#empty in
  		let dglboard = (new diagleftboard world_size)#empty in
		(Black,piecearray,horboard,verboard,dgrboard,dglboard)


  	(** Get the piece associated with a location in the world. **)
  	let get (b:board) ((x,y):index) : piece_object = 
  		let (_,pa,_,_,_,_) = b in
    	pa.(x).(y)

    let getColor (b:board) = 
    	let (p,_,_,_,_,_) = b in p

    let printcolor (b:board) =  
      match getColor b with
        |Black -> print_string " (Black) "; flush_all ()
        |White -> print_string " (White) "; flush_all ()
        |Unocc -> ()

  	(** Change the status of a piece on the board to whichever color player is**)
  	let insert (b:board) (i:index) : board = 
  		let (p,pa,h,v,dr,dl) = b in
    	match (h#insert i p, v#insert i p, 
          dr#insert i p, dl#insert i p) with
        	|(Some h1,Some v1,Some dr1,Some dl1) -> 
        		print_string "worked";
        		(let (x,y) = i in 
        			pa.(x).(y) <- (new piece p);
        			(if p = Black
        			 then (White,pa,h1,v1,dr1,dl1)
        			 else (Black,pa,h1,v1,dr1,dl1)) )
        	|_ -> (print_string "here's the problem"; b)

    let insertspecial (b:board) (i:index) (c:occupied): board = 
  		let (p,pa,h,v,dr,dl) = b in
    	match (h#insert i c, v#insert i c, 
          dr#insert i c, dl#insert i c) with
        	|(Some h1,Some v1,Some dr1,Some dl1) -> 
        		(let (x,y) = i in 
        			pa.(x).(y) <- (new piece c);
        			(p,pa,h1,v1,dr1,dl1))
        	|_ -> b

    let isWin (b:board) : bool =
    	let (_,_,h,v,dr,dl) = b in
    	(h#isWin || v#isWin || dr#isWin || dl#isWin)

  (** Remove a piece at a location **)
  (*let remove ((x,y):index) : unit = 
    ignore ((!horboard)#remove (x,y));
    ignore ((!verboard)#remove (x,y));
    ignore ((!dgrboard)#remove (x,y));
    ignore ((!dglboard)#remove (x,y));
    board.(x).(y) <- (new piece Unocc)*)

(*

  (** Fold over all objects in the world. *)
  let fold (f:world_object_i -> 'a -> 'a) (i:'a) : 'a =
    Array.fold_right 
      (fun row accum -> 
         Array.fold_right 
           (fun os accum' -> List.fold_right f os accum') 
           row 
           accum)
      world 
      i 
*)
  let getThreats (b:board) : threat list = 
  	let (_,_,h,v,dr,dl) = b in
  	(h#getThreats)@(v#getThreats)@(dr#getThreats)@(dl#getThreats)

  (** Call a function for all pieces in the world. *)
  let indices (b:board) (f:int*int -> unit) : unit =
  	let (_,pa,_,_,_,_) = b in
    (*let (a,b,c,d) = board in
    List.iter (fun x -> List.iter (fun y -> f y) x) a#getIndices*)

    Array.iteri (fun x -> Array.iteri (fun y _ -> f (x,y))) pa
(*
  (** True if the world contains the point (x,y). *)
  let check_bounds ((x,y):int*int) : bool = 
    x >= 0 && x < size && y >= 0 && y < size

  (** Iterate of all world objects along with their corresponding location. *)
  let iteri (f:int*int -> world_object_i -> unit) : unit = 
    indices (fun p -> List.iter (f p) (get p))

  (** True if the world contains no objects at point p. *)
  let is_empty p = get p = []

  (** All objects within n spaces from location (x,y). *)
  let objects_within_range ((x,y):int*int) (n:int) : world_object_i list =
    let xlow = max (x-n) 0 in
    let ylow = max (y-n) 0 in
    let xhigh = min (x+n) (size-1) in
    let yhigh = min (y+n) (size-1) in
    let coords = Helpers.cross (Helpers.range xlow xhigh) (Helpers.range ylow yhigh) in
    List.fold_right (fun p t -> get p @ t) coords []

  (** The next available point to (x,y) is a random close by element which is
      both on the grid and is unoccupied. *)
  let rec next_available ((x,y):int*int) : int*int =
    if not (check_bounds (x,y)) then 
      next_available (Helpers.bound 0 (size-1) x,Helpers.bound 0 (size-1) y)
    else if not (is_empty (x,y)) then
      next_available (Direction.move_point (x,y) (Some (Direction.random rand)))
    else (x,y)

  (****************************)
  (***** Spawning Objects *****)
  (****************************)

  (** [spawn n p f] will call f n times on points near p which are both on the
      grid and unoccupied. *)
  let rec spawn (n:int) (p:int*int) (f:int*int -> unit) : unit =
    if n <= 0 then () else 
      let p' = next_available p in
      f p' ;
      spawn (n-1) p' f

    (** [spawn_iter num num_spawn barrier f] will call [spawn n p f] num times
        where p is a randomly chosen point.  [barrier] will be called between
        spawns. *)
  let rec spawn_iter (num_iter:int) 
                     (num_spawn:int) 
                     (barrier:unit -> unit)
                     (f:int*int -> unit) : unit =
    if num_iter = 0 then () else begin
      barrier () ;
      spawn (rand num_spawn) (rand size, rand size) f ;
      spawn_iter (num_iter-1) num_spawn barrier f
    end

  (**************************)
  (***** World Movement *****)
  (**************************)

  (** True if there are any obstacles at point p. *)
  let has_obstacles (p:int*int) : bool =
    Helpers.fold_left (||) false (List.map (fun o -> o#is_obstacle) (get p))

  (** True if there are any obstacles in the path. *)
  let path_has_obstacles (ps:(int*int) list) : bool =
    Helpers.fold_left (||) false (List.map has_obstacles ps)

  (** An object can move to point p if it is in bounds and doesn't contain any
      obstacles. *)
  let can_move (p:int*int) : bool =
    check_bounds p && not (has_obstacles p)

  (** If the natural path from p1 to p2 contains no obstacles then the direction
      from p1 to p2 is the natural direction.  Otherwise it is random. *)
  let direction_from_to (p1:int*int) (p2:int*int) : Direction.direction option =
    assert (check_bounds p1 && check_bounds p2) ;
    if path_has_obstacles (Direction.natural_path p1 p2) 
    then Some (Direction.random rand)
    else Direction.natural p1 p2

  (** Gives the direction towards the object, gives random direction if there
     is an obstacle **)
  let indirect (p1:int*int) (p2:int*int) : Direction.direction option =
    let dir = Direction.natural p1 p2 in
    let next_space = Direction.move_point p1 dir in
    if has_obstacles next_space 
    then Some (Direction.random rand)
    else dir
*)
  (******************)
  (***** EVENTS *****)
  (******************)

  (** Fires when the mouse is clicked. *)
  
end

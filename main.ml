(* Main.ml Description: 						    *
 * Call draw: Draw will take user input, and return a board                 *
 * Call board: Find all threats on board from draw; create a list of board, *
 * threat pairs.							    *
 * Call on each pair in the list:					    *
 * (1) gen_threat_tree							    *
 * (2) evaluate_tree  							    *
 * from tree. If (2) evaluates to a winning sequence, create new board with *
 * first move from that sequence, and send board to draw.                   *)

open Board
open Boardobject
(*open Tree
open Draw*)
open GUI
open Boardstuffs

module MyBoard = Myboard ;;

let gridarrayhor = Array.make world_size (0,0,0,0);;
let gridarrayver = Array.make world_size (0,0,0,0);;

let floor = obj_width
let ceiling = world_size * obj_width

let draw_grid () = 
    Array.iteri (fun x _ -> gridarrayhor.(x) <- (obj_width,((x+1)*obj_width),
    		   ((world_size)*obj_width),((x+1)*obj_width))) 
    	gridarrayhor;
    Graphics.draw_segments gridarrayhor;
    Array.iteri (fun x _ -> gridarrayver.(x) <- (((x+1)*obj_width), obj_width,
    		   ((x+1)*obj_width),((world_size)*obj_width))) 
    	gridarrayver;
    Graphics.draw_segments gridarrayver

(** defines leeway for the click **)
let leeway = obj_width / 4;;

let roundfloat (f:float) : int = 
	let remainder = mod_float f 1. in
	if remainder < 0.5 then int_of_float f
	else (int_of_float f) + 1 

(** Finds the closest index that is next to the click **)
let round_click ((x,y):int*int) = 
	(abs (roundfloat ((float_of_int (x - obj_width))/.(float_of_int obj_width))), 
	abs (roundfloat ((float_of_int (y - obj_width))/.(float_of_int obj_width))))

let respond_click (b:MyBoard.board) ((x,y):int*int) = 
	if ( (x < floor - leeway) || (y < floor - leeway) ||
		(x > ceiling + leeway) || (y > ceiling + leeway) )
	then b
	else MyBoard.insert b (round_click (x,y))

let b = MyBoard.empty

let test_board () =
	GUI.run_game
		(* Initialize the board to be empty *)
		(fun () -> draw_grid ();
      				MyBoard.indices b (fun p -> (MyBoard.get b p)#draw p))
		begin fun (i:int*int) -> 
      		(*Graphics.clear_graph () ; *)
      		(* draw loop *)
      		(if MyBoard.getColor (respond_click b i) = White then print_string " it's White  ");
      		let b = respond_click b i;
      		(if MyBoard.getColor b = White then print_string " it's White  ");
      		(if MyBoard.isWin b then print_string "Win!!!! "; flush_all ());
      		(*(if MyBoard.getColor b = White then print_string " it's White  ");*)
      		draw_grid ();
      		MyBoard.indices b (fun p -> (MyBoard.get b p)#draw p)
      	end ;;

let _ = test_board () ;;

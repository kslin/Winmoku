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
open Tree
open Draw
open GUI

let test_board =
	GUI.run_game
		(* Initialize the board to be empty *)
		begin fun () -> ();
		end
		begin fun () ->
      		Graphics.clear_graph () ; 
      		Event.fire_event Board.click_event () ;
      		(* draw loop *)
      		Board.indices begin fun p -> 
        		in List.iter (fun w -> w#draw) (Board.get p)
      		end
      	end ;;

test_board () ;;

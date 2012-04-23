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
open Threats

(*module MyBoard = Myboard ;;*)

let gridarrayhor = Array.make world_size (0,0,0,0);;
let gridarrayver = Array.make world_size (0,0,0,0);;

let floor = obj_width * 2
let ceiling = (world_size + 1) * (obj_width)


let fill_board () = 
    Graphics.set_color (Graphics.rgb 204 153 51);
    Graphics.fill_rect obj_width obj_width 
    	(ceiling) (ceiling)

let board_border () =
	Graphics.set_line_width 6;
	Graphics.set_color (Graphics.rgb 102 51 0);
	Graphics.draw_rect obj_width obj_width  
	    (ceiling) (ceiling);
	Graphics.set_line_width 1

let draw_grid () = 
    Graphics.set_color (Graphics.rgb 102 51 0);
    Graphics.set_line_width 1;
    Array.iteri (fun x _ -> gridarrayhor.(x) <- (floor,((x+2)*obj_width),
    		   ceiling, ((x+2)*obj_width))) 
    	gridarrayhor;
    Graphics.draw_segments gridarrayhor;
    Array.iteri (fun x _ -> gridarrayver.(x) <- (((x+2)*obj_width), floor,
    		   ((x+2)*obj_width),(ceiling))) 
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
	(abs (roundfloat ((float_of_int (x - (2*obj_width)))/.(float_of_int obj_width))), 
	abs (roundfloat ((float_of_int (y - (2*obj_width)))/.(float_of_int obj_width))))

let print_color (b:Myboard.board) = match Myboard.getColor b with
	|Black -> print_string " Black "; flush_all ()
	|White -> print_string " White "; flush_all ()
	|Unocc -> ()

let respond_click (b:Myboard.board) ((x,y):int*int) : Myboard.board = 
	if ( (x < floor - leeway) || (y < floor - leeway) ||
		(x > ceiling + leeway) || (y > ceiling + leeway) )
	then b
	else (print_color b; print_color (Myboard.insert b (round_click (x,y)));
		(Myboard.insert b (round_click (x,y))))

let bor = Myboard.empty

let test_board () =
	GUI.run_game
		(* Initialize the board to be empty *)
		(fun () -> draw_grid ();
              fill_board ();
              draw_grid ();
              board_border ();
      				Myboard.indices bor (fun x -> (Myboard.get bor x)#draw x))
		begin fun (i:int*int) -> 
      		(*Graphics.clear_graph () ; *)
      		(* draw loop *)
      		(*(if MyBoard.getColor (respond_click b i) = White then print_string " it's White  ");*)
      		(*(if MyBoard.getColor (respond_click bor i) = MyBoard.getColor (respond_click bor i) then print_string " equal  ");
      		(if MyBoard.getColor (respond_click bor i) = Black then print_string " it's Black3  ");*)
      		ignore(bor = respond_click bor i);
      		(if Myboard.getColor bor = White then print_string " it's now White  ");
      		(if Myboard.isWin bor then print_string "Win!!!! "; flush_all ());
      		(*(if MyBoard.getColor b = White then print_string " it's White  ");*)
      		fill_board ();
          	draw_grid ();
          	board_border ();
      		Myboard.indices bor (fun p -> (Myboard.get bor p)#draw p)
      	end ;;

let evaluate_board board =
  let threatlist = BThreats.get_threats board in
  let update_board threat = 
    let Threat(_, tgain, _, _) = threat in
      ((Myboard.insertspecial board tgain Black), threat) 
  in 
  let boardlist = List.map update_board threatlist in
  let treelist = List.map (fun (x, y) -> (BThreats.gen_threat_tree x y)) 
                          boardlist in
  let rec win tlist =   
    match tlist with 
    | [] -> false
    | hd::tl -> (BThreats.evaluate_tree hd) || (win tl)
  in
    win treelist

let _ = test_board () ;;

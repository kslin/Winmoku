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
open ImportImage
open GUI
open Boardstuffs
open Threats

(*module MyBoard = Myboard ;;*)
let gridarrayhor = Array.make world_size (0,0,0,0);;
let gridarrayver = Array.make world_size (0,0,0,0);;

let floor = obj_width * 2
let ceiling = (world_size + 1) * (obj_width)

(* Stores the color *)
let piece_color = ref (Unocc)

(* Stores the current board *)
let bor  = Myboard.empty

(* Ref to point to board *)
let ref_bor = ref bor

(** defines leeway for the click **)
let leeway = obj_width / 4

(* Stores if current board already won *)
let won_board = ref false

(* Fills the board background with color *)
let board_fill () = 
    Graphics.set_color (Graphics.rgb 204 153 51);
    Graphics.fill_rect obj_width obj_width 
    	(ceiling) (ceiling)

(* Draws board border *)
let board_border () =
	Graphics.set_line_width 6;
	Graphics.set_color (Graphics.rgb 102 51 0);
	Graphics.draw_rect obj_width obj_width  
	    (ceiling) (ceiling);
	Graphics.set_line_width 1

(* Displays game title *)
let board_title () = 
  Graphics.moveto  ((world_size + 3) * obj_width / 2) 
    ((world_size+7) * obj_width);
  Graphics.draw_string "GOMOOKU"

(* Displays the player currently making a move *)
let board_player () = 
  let player : string = 
    match !piece_color with
      | Unocc -> "-"
      | White -> "White"
      | Black -> "Black" in
  Graphics.moveto (obj_width * 15) ((world_size+3) * obj_width);
  Graphics.draw_string ("Current Player:" ^ player)

(* Sets white color for next piece *)
let board_set_white () =
  Graphics.set_color (Graphics.rgb 102 51 0);
  Graphics.moveto  (obj_width) ((world_size+4) * obj_width);
  Graphics.draw_string "Change piece color";
  Graphics.set_color Graphics.black;
  Graphics.draw_rect (obj_width ) ((world_size+3) * obj_width) 
    (obj_width) (obj_width)

(* Sets black color for next piece *)
let board_set_black () =
  Graphics.set_color Graphics.black;
  Graphics.fill_rect (obj_width * 2) ((world_size+3) * obj_width) (obj_width) 
    (obj_width)

(* Draws the playing grid of the board *)
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

(* Function to draw basic components of board by compiling various functions *)
let draw_board () =   
  board_fill ();
  draw_grid ();
  board_border ();
  board_title ();
  board_player ();
  board_set_white ();
  board_set_black ()

let roundfloat (f:float) : int = 
	let remainder = mod_float f 1. in
	if remainder < 0.5 then int_of_float f
	else (int_of_float f) + 1 

(* Evaluate board function *)
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

(*  button for eval function *)
let debug_button_eval () =
  Graphics.set_color Graphics.red;
  Graphics.moveto  (obj_width) ((world_size+6) * obj_width);
  Graphics.draw_string "Debug function eval";
  Graphics.fill_rect obj_width ((world_size+5) * obj_width) (2 * obj_width) 
    (obj_width)

(* Shows buttons and other displays for function testing purposes *)
let debug_board () = 
  debug_button_eval ()

(** Finds the closest index that is next to the click **)
let round_click ((x,y):int*int) = 
  (abs (roundfloat ((float_of_int (x - (2*obj_width)))/.(float_of_int obj_width))), 
  abs (roundfloat ((float_of_int (y - (2*obj_width)))/.(float_of_int obj_width))))

let respond_click (b:Myboard.board) ((x,y):int*int) : Myboard.board = 

  if ( (x < floor - leeway) || (y < floor - leeway) ||
    (x > ceiling + leeway) || (y > ceiling + leeway) )
  then b
  else (
    (Myboard.insertspecial b (round_click (x,y))) !piece_color)

(* A handles clicks to to run functions in the area above the board: 
  debugging function, change piece color *)
let respond_click_header (b:Myboard.board) ((x,y):int*int) = 
  if ((x > obj_width) && (x < 3 * obj_width) 
    && (y > ((world_size+5) * obj_width)) 
    && (y < ((world_size+6) * obj_width)))
  then (let result = evaluate_board b in
        print_string (string_of_bool result); flush_all ())
  else if ((x > obj_width) && (x < 2 * obj_width) 
    && (y > ((world_size+3) * obj_width)) 
    && (y < ((world_size+4) * obj_width)))
  then ((piece_color := White))
  else if ((x > (2 *obj_width)) && (x < 3 * obj_width) 
    && (y > ((world_size+3) * obj_width)) 
    && (y < ((world_size+4) * obj_width))) 
  then ((piece_color := Black))
  else ()

(* Compiles everything to be drawn in window *)
let draw_all () : unit =
  draw_board ();
  debug_board ()

let test_board () =
	GUI.run_game
		(* Initialize the board to be empty *)
    (fun () ->
      Graphics.clear_graph ();
      draw_all ();
      Myboard.indices bor (fun x -> (Myboard.get bor x)#draw x))
    (* function for handling key presses *)
    (fun (c:char) -> 
      match c with 
      | 'r' -> ignore (ref_bor := (Myboard.get_empty ())); 
        (* print_string (string_of_bool (bor = Myboard.empty)); flush_all (); *)
        won_board := false; 
        Graphics.clear_graph ();
        draw_all (); 
        Myboard.indices bor (fun x -> (Myboard.get bor x)#draw x)
      | 'R' -> ignore (ref_bor := (Myboard.get_empty ())); 
        won_board := false; 
        Graphics.clear_graph ();
        draw_all (); 
        Myboard.indices bor (fun x -> (Myboard.get bor x)#draw x)
      | _ -> () 
    ) 
    (* function for handling mouse click events *)
		(fun (i:int*int) -> 
        (* If mouse click is in the area above the playing grid, checks the 
        click position to do other things such as running a debugging function*)
        if !won_board then () else (
        if (snd i) > ceiling then (
          respond_click_header bor i;
          Graphics.clear_graph ();
          draw_all ();
          Myboard.indices bor (fun x -> (Myboard.get bor x)#draw x))
        (* If mouse clicks on board area, make a move *)          
        else (
      		ignore(bor = respond_click bor i);
      		(if Myboard.isWin bor then (
            let player : string = 
              match !piece_color with
              | Unocc -> "-"
              | White -> "WHITE"
              | Black -> "BLACK" in
            won_board := true;
            (Graphics.set_color Graphics.red);
            Graphics.moveto (obj_width * 15) ((world_size+5) * obj_width);
            Graphics.draw_string (player ^ " " ^ "WON!!!")));
          draw_all ();
          Myboard.indices bor (fun x -> (Myboard.get bor x)#draw x) )))

let _ = test_board () ;;

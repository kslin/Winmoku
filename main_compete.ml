(* Main.ml Description:                 *
 * Call draw: Draw will take user input, and return a board                 *
 * Call board: Find all threats on board from draw; create a list of board, *
 * threat pairs.                  *
 * Call on each pair in the list:             *
 * (1) gen_threat_tree                  *
 * (2) evaluate_tree                    *
 * from tree. If (2) evaluates to a winning sequence, create new board with *
 * first move from that sequence, and send board to draw.                   *)

open Board
open ImportImage
open GUI
open Boardstuffs
open Threats
open Mainhelpers

(* Stores if current board already won *)
let won_board = ref false


(* Function to draw basic components of board by compiling various functions *)
let draw_board () =   
  board_fill ();
  draw_grid ();
  board_border ();
  board_title ();
  draw_coord ()

(* Respond to a click, insert pieces if the click is near an index *)
let respond_click (b:Myboard.board) ((x,y):int*int) : Myboard.board option = 
  let (rx, ry) = round_click (x,y) in
  if ( (x < floor - leeway) || (y < floor - leeway) ||
    (x > ceiling + leeway) || (y > ceiling + leeway) )
    || (Myboard.get b (rx,ry) <> Unocc)
  then None
  else (
    Some (Myboard.insertspecial b (rx,ry) White))
 (*)
(* Evaluate board function *)
let evaluate_board board =
  let threatlist = BThreats.get_threats board in
  let update_board threat = 
    let Threat(_, (x,y), a, _) = threat in
      (print_string ((string_of_int x) ^ "," ^ (string_of_int y) ^ ":");
       flush_all ();
       List.map (fun z -> let (c,d) = z in
       begin  
         print_string ((string_of_int c) ^ "," ^ (string_of_int d) ^ "|");
         flush_all ();
       end) a;
      ((Myboard.insertspecial board (x,y) Black), threat))
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
  *)
      

(*  button for eval function *)
let debug_button_eval () =
  Graphics.set_color Graphics.red;
  Graphics.moveto  (obj_width) ((world_size+6) * obj_width);
  Graphics.draw_string "Debug function eval";
  Graphics.fill_rect obj_width ((world_size+5) * obj_width) (2 * obj_width) (obj_width)

(* Shows buttons and other displays for function testing purposes *)
let debug_board () = 
  debug_button_eval ()

(*)
(* A handles clicks to to run functions in the area above the board: 
  debugging function, change piece color *)
let respond_click_header (b:Myboard.board) ((x,y):int*int) = 
  if ((x > obj_width) && (x < 3 * obj_width) && (y > ((world_size+5) * obj_width)) 
    && (y < ((world_size+6) * obj_width)))
  then (let result = evaluate_board b in
        print_string (string_of_bool result); flush_all ())
  else ()
*)

(* Determines the next move based on threats *)
let next_move (b:Myboard.board) : int*int =
  match Myboard.nextWin b with
    |Some s -> s
    |None -> (Random.int (world_size-1), Random.int (world_size-1))

(* First move of the game *)
let firstmove = ((world_size/2), (world_size/2))

(* Run the board *)
let test_board () =
  GUI.run_game
    (* Initialize the board to be empty *)
    begin fun (bor:Myboard.board) -> 
      draw_board ();
      debug_board ();
      let newbor = Myboard.insertspecial bor firstmove Black in
      Myboard.indices newbor;
      newbor
    end
    begin fun (bor:Myboard.board) -> 
      Graphics.clear_graph ();
      won_board := false;
      draw_board ();
      debug_board ();
      let newbor = Myboard.insertspecial Myboard.empty firstmove Black in
      Graphics.clear_graph ();
      draw_board ();
      debug_board ();
      Myboard.indices newbor;
      newbor
    end
    begin fun (bor:Myboard.board) (i:int*int) -> 
        (* If mouse click is in the area above the playing grid, checks the 
        click position to do other things such as running a debugging function*)
        if !won_board 
        then bor 
        else (
          if ((snd i) > ceiling || (snd i) < floor ||
	    (fst i) > ceiling || (fst i) < floor)
          then (
            Graphics.clear_graph ();
            draw_board ();
            debug_board ();
            Myboard.indices bor;
            bor
          )
          (* If mouse clicks on board area, make a move *)          
          else (
            let newbor = respond_click bor i in
            match newbor with
	      | None ->
		Graphics.clear_graph ();
		draw_board ();
		debug_board ();
		Myboard.indices bor;
		bor
	      | Some newbor1 ->
		(match Myboard.isWin newbor1 with
		  |Some s ->
                    won_board := true;
                    (Graphics.set_color Graphics.red);
                    Graphics.moveto (obj_width * 15) ((world_size+5) * obj_width);
                    Graphics.draw_string ("WHITE WON!!!");
                    Myboard.indices newbor1;
                    newbor1
		  |None -> 
                    let next = next_move newbor1 in
                    let newbor2 = Myboard.insertspecial newbor1 next Black in
                    (match Myboard.isWin newbor2 with
                      |Some s ->
			won_board := true;
			(Graphics.set_color Graphics.red);
			Graphics.moveto (obj_width * 15) ((world_size+5) * obj_width);
			Graphics.draw_string ("BLACK WON!!!")
                      |None -> ()
                    );
                    Myboard.indices newbor2;
                    newbor2
		)
          )
        )
    end 
  ;;

let _ = test_board () ;;

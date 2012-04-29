(* This version of the game allows people to play each other.
   It does not evaluate threats *)

open Board
open ImportImage
open GUI
open Boardstuffs
open Mainhelpers

(* Stores the color *)
let piece_color = ref (Black)

(* Stores if current board already won *)
let won_board = ref false

(* Switches the color of the current player *)
let switch_color () = match !piece_color with
  |Black -> piece_color := White
  |White -> piece_color := Black
  |_ -> piece_color := Black

(* Displays the player currently making a move *)
let board_player () = 
  let player : string = 
    match !piece_color with
      | Unocc -> "-"
      | White -> "White"
      | Black -> "Black" in
  Graphics.moveto (obj_width * 15) ((world_size+3) * obj_width);
  Graphics.draw_string ("Current Player:" ^ player)


(* Function to draw basic components of board by compiling various functions *)
let draw_board () =   
  board_fill ();
  draw_grid ();
  board_border ();
  board_title ();
  board_player ();
  draw_coord ()

(* Respond to a click, insert pieces if the click is near an index *)
let respond_click (b:Myboard.board) ((x,y):int*int) : Myboard.board = 

  if ( (x < floor - leeway) || (y < floor - leeway) ||
    (x > ceiling + leeway) || (y > ceiling + leeway) )
  then b
  else (
    (Myboard.insertspecial b (round_click (x,y))) !piece_color)

(*  button for eval function *)
let debug_button_eval () =
  Graphics.set_color Graphics.red;
  Graphics.moveto  (obj_width) ((world_size+6) * obj_width);
  Graphics.draw_string "Debug function eval";
  Graphics.fill_rect obj_width ((world_size+5) * obj_width) (2 * obj_width) (obj_width)

(* Shows buttons and other displays for function testing purposes *)
let debug_board () = 
  debug_button_eval ()

(* Run the board *)
let test_board () =
  GUI.run_game
    (* Initialize the board to be empty *)
    begin fun (bor:Myboard.board) -> 
      draw_board ();
      debug_board ();
      Myboard.indices bor;
      bor
    end
    (* Reset the board to be empty *)
    begin fun (bor:Myboard.board) -> 
      Graphics.clear_graph ();
      piece_color := Unocc;
      won_board := false;
      Graphics.clear_graph ();
      draw_board ();
      debug_board ();
      let newbor = Myboard.empty in
      Myboard.indices newbor;
      newbor
    end
    begin fun (bor:Myboard.board) (i:int*int) -> 
        (* If mouse click is in the area above the playing grid, checks the 
        click position to do other things such as running a debugging function*)
        if !won_board 
        then bor 
        else (
          if (snd i) > ceiling 
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
            let threats = Myboard.getThreats newbor in
            switch_color ();
            Graphics.clear_graph ();
            (* Checks if the board has a winning sequence 
              Stops the game and prints the winner if there is a win *)
            (match Myboard.isWin newbor with
              |None -> ()
              |Some s -> 
                let player : string = 
                match s with
                  | Unocc -> "-"
                  | White -> "WHITE"
                  | Black -> "BLACK" in
                won_board := true;
                (Graphics.set_color Graphics.red);
                Graphics.moveto (obj_width * 15) ((world_size+5) * obj_width);
                Graphics.draw_string (player ^ " " ^ "WON!!!")
            );
            debug_board ();
            draw_board ();
            Myboard.indices newbor;
            newbor
          )
        )
    end 
  ;;

let _ = test_board () ;;

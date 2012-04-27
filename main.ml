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


(* Stores the color *)
let piece_color = ref (Unocc)

(* Stores if current board already won *)
let won_board = ref false

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
  board_set_white ();
  board_set_black ();
  draw_coord ()

(* Respond to a click, insert pieces if the click is near an index *)
let respond_click (b:Myboard.board) ((x,y):int*int) : Myboard.board = 
  if ( (x < floor - leeway) || (y < floor - leeway) ||
    (x > ceiling + leeway) || (y > ceiling + leeway) )
  then b
  else (
    (Myboard.insertspecial b (round_click (x,y))) !piece_color)

(* Evaluate board function *)
let evaluate_board board =
  let threatlist = BThreats.get_threats board in
  let update_board threat = 
    ((BThreats.gen_new_board board threat), threat)
  in 
  let boardlist = List.map update_board threatlist in
  let treelist = List.map (fun (x, y) -> (BThreats.gen_threat_tree x y [])) 
                          boardlist in 
  let rec win tlist =   
    match tlist with 
    | [] -> None
    | hd::tl -> (if BThreats.evaluate_tree hd = None then (win tl)
                else BThreats.evaluate_tree hd)
  in
    win treelist
      

(*  button for eval function *)
let debug_button_eval () =
  Graphics.set_color Graphics.red;
  Graphics.moveto  (obj_width) ((world_size+6) * obj_width);
  Graphics.draw_string "Debug function eval";
  Graphics.fill_rect obj_width ((world_size+5) * obj_width) (2 * obj_width) (obj_width)

(* Shows buttons and other displays for function testing purposes *)
let debug_board () = 
  debug_button_eval ()

let rec print_threatlist tlist = 
  match tlist with
  | [] -> ()
  | hd::tl -> (print_threats hd; print_threatlist tl)

(* A handles clicks to to run functions in the area above the board: 
  debugging function, change piece color *)
let respond_click_header (b:Myboard.board) ((x,y):int*int) = 
  if ((x > obj_width) && (x < 3 * obj_width) && (y > ((world_size+5) * obj_width)) 
    && (y < ((world_size+6) * obj_width)))
  then (let result = evaluate_board b in
        match result with
        | None -> (print_string "None"; flush_all();)
        | Some tlist -> print_threatlist tlist)
  else if ((x > obj_width) && (x < 2 * obj_width) && (y > ((world_size+3) * obj_width)) 
    && (y < ((world_size+4) * obj_width)))
  then ((piece_color := White))
  else if ((x > (2 *obj_width)) && (x < 3 * obj_width) && (y > ((world_size+3) * obj_width)) 
    && (y < ((world_size+4) * obj_width))) 
  then ((piece_color := Black))
  else ()

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
    begin fun (bor:Myboard.board) -> 
      Graphics.clear_graph ();
      piece_color := Unocc;
      won_board := false;
      draw_board ();
      debug_board ();
      Myboard.indices bor;
      bor
    end
    begin fun (bor:Myboard.board) (i:int*int) -> 
        (* If mouse click is in the area above the playing grid, checks the 
        click position to do other things such as running a debugging function*)
        if !won_board 
        then bor 
        else (
          if (snd i) > ceiling 
          then (
            respond_click_header bor i;
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
            List.iter (print_threats) threats;
            print_string "threats: "; print_int (List.length threats);
            print_string "\n";
            flush_all ();
            newbor
          )
        )
    end 
  ;;

let _ = test_board () ;;

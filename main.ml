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
open GUI
open Boardstuffs
open Threats
open Mainhelpers
open SequenceBoards

let piece_color = ref (Unocc)

(* Stores if current board already won *)
let won_board = ref false

(* Stores if we're displaying threats *)
let displaythreats = ref false

(* Stores if we're playing next move *)
let displaymove = ref false

(* Stores if there is a straight four *)
let board_four = ref false

(* Stores the winning threat after board has been set *)
let win_seq = ref []

(* Stores the current preset board number *)
let board_num = ref 0

let pboard = ref false

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
  board_instruct ();
  draw_coord ()

(* Respond to a click, insert pieces if the click is near an index *)
let respond_click (b:Myboard.board) ((x,y):int*int) : Myboard.board = 
  if ( (x < floor - leeway) || (y < floor - leeway) ||
    (x > ceiling + leeway) || (y > ceiling + leeway) )
  then b
  else (
    (Myboard.insertspecial b (round_click (x,y))) !piece_color)

let winning_move board = 
  print_string "here";
  let threatlist = BThreats.get_threats board in
  print_string "1";
  let update_board threat = 
    ((BThreats.gen_new_board board threat), threat)
  in 
  print_string "2";
  let boardlist = List.map update_board threatlist in
  print_string "3";
  let treelist = List.map (fun (x, y) -> (BThreats.gen_threat_tree x y [])) 
                          boardlist in 
  print_string "4";
  let rec win tlist =   
    match tlist with 
    | [] -> None
    | hd::tl -> (let result = BThreats.next_winning_move hd in
		 if result = None then (win tl) else result)
  in
    win treelist
      
(* Evaluate board function *)

let evaluate_board board = BThreats.evaluate_board board

(*  button for getting the next pre-setboard *)
let button_bor () =
  Graphics.set_color Graphics.blue;
  Graphics.moveto  (obj_width) ((world_size+6) * obj_width);
  Graphics.draw_string "Next Board";
  Graphics.fill_rect obj_width ((world_size+5) * obj_width) 
    (2 * obj_width) (obj_width)

(* button to run the threat-space search algorithm on the current board *)
let button_getwin () = 
  Graphics.set_color Graphics.red;
  Graphics.moveto  (obj_width *4) ((world_size+6) * obj_width);
  Graphics.draw_string "Get Win";
  Graphics.fill_rect (obj_width*4) ((world_size+5) * obj_width) 
    (2 * obj_width) (obj_width) 

(* button to make the next move of the winning threat sequence*)
let button_nextmove () = 
  Graphics.set_color Graphics.green;
  Graphics.moveto  (obj_width *7) ((world_size+6) * obj_width);
  Graphics.draw_string "Play Win";
  Graphics.fill_rect (obj_width*7) ((world_size+5) * obj_width) 
    (2 * obj_width) (obj_width) 

(* Shows buttons and other displays for function testing purposes *)
let debug_board () = 
  button_bor ();
  button_getwin ();
  button_nextmove ()

(* Converts a threat list (like the winning sequence) into a list of 
(bool, list of moves) where the bool indicates if we've reached a straight four
and have won and the list is a list of moves that can be inserted into the 
board*)
let rec extract_win_seq tlist winlist = 
  let rec extract_tcost clist costlist =
    match clist with
    | [] -> costlist
    | h::t -> extract_tcost t ((h, White)::costlist) in
  match tlist with 
  | [] -> winlist
  | Threat(StraightFour,tgain,tcost,_)::t -> extract_win_seq t 
    ((false, [(tgain, Black)]) :: ((true,(extract_tcost tcost [])) :: 
    winlist))
  | Threat(_,tgain,tcost,_)::t -> extract_win_seq t 
    ((false, [(tgain, Black)]) :: ((false,(extract_tcost tcost [])) :: 
    winlist))

(* inserts a list of (index, occupied) into board and returns the board *)
let rec insertlist b lst = 
  match lst with
  | [] -> b
  | h::t -> insertlist (Myboard.insertspecial b (fst h) (snd h)) t

(* plays the next move in the winning sequences, updates the winning sequence
with the remaining moves. Returns the original board if no more moves *)
let play_next bor =
  match (!win_seq) with
  | [] -> (print_string "no more winning moves"; flush_all()); bor
  | h::t -> 
    if fst h then (board_four := true;
      (print_string "straight four!"; flush_all()); bor)
    else ((win_seq := t); insertlist bor (snd h))

(* displays threats when given a list of threats *)
let rec print_threatlist tlist = 
  match tlist with
  | [] -> ()
  | hd::tl -> (print_threats hd; print_threatlist tl)

(* converts a pair of (occ, index) to string *)
let rec print_pairs l = 
  match l with
  | [] -> ""
  | (i, c)::[] -> (string_of_occ c) ^ (string_of_index i)
  | (i, c)::t -> (string_of_occ c) ^ (string_of_index i) ^ "," ^ 
                 (print_pairs t)

(* Displays a sequence of threats on the side *)
let threat_display () = 
  let rec show_threat l num =
    match l with
    | [] -> ()
    | (b, lst)::t -> 
      (Graphics.moveto ((world_size+4)*obj_width) (num*obj_width));
      (Graphics.draw_string (print_pairs lst));
      (Graphics.set_color Graphics.black);
      (show_threat t (num-1)) in
  if !win_seq = [] then
    (Graphics.set_color Graphics.red;
    (Graphics.moveto ((world_size+4)*obj_width) ((world_size+7)*obj_width));
    (Graphics.draw_string "No winning sequence")) else
    ((Graphics.set_color Graphics.black);
    (Graphics.moveto ((world_size+4)*obj_width) ((world_size+7)*obj_width));
    (Graphics.draw_string "Winning sequence: "); 
    Graphics.set_color Graphics.red;
    (show_threat (!win_seq) (world_size+6)))

(* Gets the next preset board and returns it *)
let display_numboard () =
  let x = !board_num in
  if x = ((List.length (threatseq)) -1) then board_num := 0 
    else board_num := (x + 1)

(* A handles clicks to to run functions in the area above the board: 
  debugging function, change piece color *)
let respond_click_header (b:Myboard.board) ((x,y):int*int) = 
  (* gives the next pre-set board *)
  if ((x > obj_width) && (x < 3 * obj_width) 
    && (y > ((world_size+5) * obj_width)) 
    && (y < ((world_size+6) * obj_width)))
  then (pboard := true; 
    display_numboard ())
  (* gets the winning sequence after the board is set *)
  else if ((x > obj_width * 4) && (x < 6 * obj_width) 
    && (y > ((world_size+5) * obj_width)) 
    && (y < ((world_size+6) * obj_width)))
  then 
    ((displaythreats := true);
    (match (evaluate_board b) with
      | None -> 
      	print_string "None\n"; flush_all ();
      	win_seq := []; ()
      | Some t ->
      (let l = (extract_win_seq t []) in win_seq := l;
      (print_string "got threats in seq"; flush_all ()))))
  (* plays the next move in the winning sequence determined when the board
  was set *)
  else if ((x > obj_width * 7) && (x < 9 * obj_width) 
    && (y > ((world_size+5) * obj_width)) 
    && (y < ((world_size+6) * obj_width)))
  then
    displaymove := true
  (* changes player to white *)
  else if ((x > obj_width) && (x < 2 * obj_width) 
    && (y > ((world_size+3) * obj_width)) 
    && (y < ((world_size+4) * obj_width)))
  then ((piece_color := White))
  (* changes player to black *)
  else if ((x > (2 *obj_width)) && (x < 3 * obj_width) 
    && (y > ((world_size+3) * obj_width)) 
    && (y < ((world_size+4) * obj_width))) 
  then ((piece_color := Black))
  else ()

(* Run the board *)
let test_board () =
  GUI.run_game
    (* Initialize the board to a predetermined board *)
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
      board_four := false;
      pboard := false;
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
            respond_click_header bor i;
            Graphics.clear_graph ();
            draw_board ();
            debug_board ();
            (if (!displaythreats) then
              threat_display ()
              );  
            Myboard.indices bor;          
            (if (!displaymove) then 
              (let newbor = (play_next bor) in Myboard.indices newbor; 
                displaymove := false; 
                (if (!board_four) then ((Graphics.set_color Graphics.red);
                (Graphics.moveto (obj_width * 8) ((world_size+3) * obj_width));
                (Graphics.draw_string "STRAIGHT FOUR!!!")));
                newbor)
            else if (!pboard) then 
              (let newbor = List.nth threatseq !board_num in
              Graphics.clear_graph (); 
              draw_board ();
              debug_board ();
              Myboard.indices newbor;
              print_int !board_num;
              won_board := false;
              board_four := false;
              newbor)
            else bor)
          )
          (* If mouse clicks on board area, make a move *)          
          else (
            let newbor = respond_click bor i in
            let threats = Myboard.getThreats newbor in
            print_threat_list threats;
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
                Graphics.moveto (obj_width * 8) ((world_size+3) * obj_width);
                Graphics.draw_string (player ^ " " ^ "WON!!!")
            );
            debug_board ();
            draw_board ();
            (if (!displaythreats) then
              threat_display ()
              );  
            Myboard.indices newbor;
            newbor
          )
        )
    end 
  ;;

let _ = test_board () ;;

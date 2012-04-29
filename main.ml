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

let piece_color = ref (Unocc)

(* Stores if current board already won *)
let won_board = ref false

(* Stores if we're displaying threats *)
let displaythreats = ref false

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

(*  button for eval function *)
let button_eval () =
  Graphics.set_color Graphics.red;
  Graphics.moveto  (obj_width) ((world_size+6) * obj_width);
  Graphics.draw_string "Debug eval";
  Graphics.fill_rect obj_width ((world_size+5) * obj_width) (2 * obj_width) (obj_width)

let button_threat () =
  Graphics.set_color Graphics.blue;
  Graphics.moveto  (obj_width *5) ((world_size+6) * obj_width);
  Graphics.draw_string "Show Threats";
  Graphics.fill_rect (obj_width*5) ((world_size+5) * obj_width) (2 * obj_width) (obj_width)

let button_playalgo () = 
  Graphics.set_color Graphics.green;
  Graphics.moveto  (obj_width *9) ((world_size+6) * obj_width);
  Graphics.draw_string "Play Algo";
  Graphics.fill_rect (obj_width*9) ((world_size+5) * obj_width) (2 * obj_width) (obj_width) 


(* Shows buttons and other displays for function testing purposes *)
let debug_board () = 
  button_eval ();
  button_threat ();
  button_playalgo ()

let rec play_sequence b tlist =
  let rec insertwlist b ilist = 
    match ilist with
    | [] -> b
    | hd::tl -> insertwlist (Myboard.insertspecial b hd White) tl
  in
    match tlist with
    | [] -> []
    | Threat(_,tgain,tcost, _)::tl -> 
      (let bor1 = Myboard.insertspecial b tgain Black in
        (Myboard.indices bor1;
         (let bor2 = insertwlist bor1 tcost in
            (Myboard.indices bor2;
             tl))))

let play_move (b:Myboard.board) : Myboard.board =
  let win_move = winning_move b in
  let next_move = 
    match Myboard.nextWin b with
      | Some i -> i
      | None ->
	(match win_move with
	  | None -> (Random.int (world_size-1), Random.int (world_size-1))
	  | Some Threat(_,tgain,_,_) -> tgain)
  in
  print_string "Move: " ;
  print_string (string_of_int (fst next_move)) ;
  print_string ",";
  print_string (string_of_int (snd next_move)) ;
  print_string "\n" ;
  flush_all () ;
  let newbor = Myboard.insertspecial b next_move Black in
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
  debug_board () ;
  draw_board () ;
  Myboard.indices newbor ;
  newbor
                                     

let rec print_threatlist tlist = 
  match tlist with
  | [] -> ()
  | hd::tl -> (print_threats hd; print_threatlist tl)

let print_gainlist_screen glist = 
  match glist with
  | [] -> ()
  | h::t -> (Graphics.rmoveto 0 (-obj_width));(Graphics.draw_string (gain_string h))


(* A handles clicks to to run functions in the area above the board: 
  debugging function, change piece color *)
let respond_click_header (b:Myboard.board) ((x,y):int*int) = 
  (* runs eval board function *)
  if ((x > obj_width) && (x < 3 * obj_width) && (y > ((world_size+5) * obj_width)) 
    && (y < ((world_size+6) * obj_width)))
  then (let result = evaluate_board b in
        match result with
        | None -> (print_string "None\n"; flush_all();)
        | Some tlist -> print_threatlist tlist)
  (* shows current threats on the game display *)
  else if ((x > obj_width * 5) && (x < 7 * obj_width) && (y > ((world_size+5) * obj_width)) 
    && (y < ((world_size+6) * obj_width)))
  then ((print_string "button working";flush_all());
    (displaythreats := true);)
  (* shows next winning move if exists *)
  else if ((x > obj_width * 9) && (x < 11 * obj_width) && (y > ((world_size+5) * obj_width)) 
    && (y < ((world_size+6) * obj_width)))
  then 
    ignore(play_move b) 
  (* changes player to white *)
  else if ((x > obj_width) && (x < 2 * obj_width) && (y > ((world_size+3) * obj_width)) 
    && (y < ((world_size+4) * obj_width)))
  then ((piece_color := White))
  (* changes player to black *)
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
	    (* hacky plays the next move *)
	    if ((fst i > obj_width * 9) && (fst i < 11 * obj_width) && (snd i > ((world_size+5) * obj_width)) && (snd i < ((world_size+6) * obj_width)))
	    then 
	      play_move bor

	    else (
            respond_click_header bor i;
            Graphics.clear_graph ();
            draw_board ();
            debug_board ();
            Myboard.indices bor;  
            (if (!displaythreats) then
              (Graphics.moveto  (obj_width *7) ((world_size+6) * obj_width));
              (*(print_gainlist_screen bor)*));
            bor )
          )
          (* If mouse clicks on board area, make a move *)          
          else (
            let newbor = respond_click bor i in
            let threats = Myboard.getWhiteThreats newbor in
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

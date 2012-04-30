(*  This runs the complete AI *)

open Board
open GUI
open Boardstuffs
open Threats
open Mainhelpers
open Minimax

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

(* Evaluate board function; first check for winning threat sequences.
   If there aren't any, then look for hidden threats and play those. 
   If that isn't available, use the minimax algorithm. *)
let evaluate_board board = 
  match BThreats.evaluate_board board with
  | Some tlist -> (let Threat(_,tgain, _, _) = 
                     List.nth tlist ((List.length tlist) - 1) 
                   in
                     Some tgain)
  | None -> (match BThreats.hidden_threats board with
             | hd::tl -> Some hd
             | [] -> 
               (let tree1 = GMinimax.gen_tree (GMinimax.depth) None board in
                let tree2 = GMinimax.minimax tree1 in
                GMinimax.next_move tree2)) 

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

(* Determines the next move based on evaluate board defined above. First,
   make sure white doesn't have an impending threat, and also play any
   move that will win the game for you. Otherwise, play what evaluate board
   tells you to. *)
let next_move (b:Myboard.board) : int*int =
  (Random.self_init ();
    (match Myboard.nextWin b with
    | Some i -> i
    | None -> 
      (match Myboard.nextWhiteWin b with
 		   | Some i -> i
 		   | None -> 
         (let block_white = List.fold_left 
           (fun gains wt -> let Threat(wttype,wtgain,_,_) = wt in 
   				                    if wttype = StraightFour then wtgain::gains 
                              else gains)
 			    [] (Myboard.getWhiteThreats b) in
         (match block_white with
          | hd :: tl -> hd
          | [] -> 
            (match (evaluate_board b) with 
             | Some s -> (flush_all (); s)
             | None -> 
                (Random.int (world_size-1),Random.int (world_size-1))))))))

(* First move of the game *)
let firstmove = ((world_size/2), (world_size/2))

(* Run the board *)
let test_board () =
  GUI.run_game
    (* Initialize the board to be empty except for the first piece *)
    begin fun (bor:Myboard.board) -> 
      draw_board ();
      debug_board ();
      let newbor = Myboard.insertspecial bor firstmove Black in
      Myboard.indices newbor;
      newbor
    end
    (* A resetted board should be empty except for the first piece *)
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
              |None ->
            		Graphics.clear_graph ();
            		draw_board ();
            		debug_board ();
            		Myboard.indices bor;
            		bor
              |Some newbor1 ->
                Myboard.indices newbor1;
            		(match Myboard.isWin newbor1 with
            		  |Some s ->
                    won_board := true;
                    (Graphics.set_color Graphics.red);
                    Graphics.moveto (obj_width * 15) 
                                    ((world_size+5)*obj_width);
                    Graphics.draw_string ("WHITE WON!!!");
                    newbor1
		              |None -> 
                    let next = next_move newbor1 in
                    let newbor2 = Myboard.insertspecial newbor1 next Black in
                    (match Myboard.isWin newbor2 with
                      |Some s ->
                  			won_board := true;
                  			(Graphics.set_color Graphics.red);
                  			Graphics.moveto (obj_width * 15) 
                                        ((world_size+5) * obj_width);
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

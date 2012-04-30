(** Helper functions for drawing the board that are used by all main files **)
open Boardstuffs

(** Defines the lines of the board **)
let gridarrayhor = Array.make world_size (0,0,0,0);;
let gridarrayver = Array.make world_size (0,0,0,0);;

(** defines leeway for the click **)
let leeway = obj_width / 4

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
  Graphics.moveto  ((world_size + 3) * obj_width / 2) ((world_size+7) * obj_width);
  Graphics.draw_string "GOMOOKU"

(* Sets white color for next piece *)
let board_set_white () =
  Graphics.set_color (Graphics.rgb 102 51 0);
  Graphics.moveto  (obj_width) ((world_size+4) * obj_width);
  Graphics.draw_string "Change piece color";
  Graphics.set_color Graphics.black;
  Graphics.draw_rect (obj_width ) ((world_size+3) * obj_width) (obj_width) (obj_width)

(* Sets black color for next piece *)
let board_set_black () =
  Graphics.set_color Graphics.black;
  Graphics.fill_rect (obj_width * 2) ((world_size+3) * obj_width) (obj_width) (obj_width)

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

(* Writes out the instructions *)
let board_instruct () = 
  let h = (world_size+6) * obj_width -10 in
  let x = 11 in
  Graphics.set_color (Graphics.rgb 102 51 0);
  Graphics.moveto  (obj_width * 10) (h);
  Graphics.draw_string "Press 'r' or 'R' to reset the board";
  Graphics.moveto  (obj_width * 10) (h - x);
  Graphics.draw_string "Click ";
  Graphics.set_color Graphics.blue;
  Graphics.draw_string "Next Board ";
  Graphics.set_color (Graphics.rgb 102 51 0);
  Graphics.draw_string "to get the next preset board";
  Graphics.moveto  (obj_width * 10) (h - 2 *x);
  Graphics.draw_string "Click ";
  Graphics.set_color Graphics.red;
  Graphics.draw_string "Get Win ";
  Graphics.set_color (Graphics.rgb 102 51 0);
  Graphics.draw_string "to get the winning sequence";
  Graphics.moveto  (obj_width * 10) (h - 3 * x);
  Graphics.draw_string "Click ";
  Graphics.set_color Graphics.green;
  Graphics.draw_string "Play Win ";
  Graphics.set_color (Graphics.rgb 102 51 0);
  Graphics.draw_string "to play next move of the ";
  Graphics.moveto (obj_width * 11) (h - 4*x);
  Graphics.draw_string "winning sequence"

(* Draws the coordinates for the board *)
let draw_coord () =
  let leeway1 = 2 in
  let leeway2 = 5 in
  Graphics.moveto ((obj_width * 2)-leeway1) ((obj_width*2)-15);
  let rec draw_x n =
    if n = world_size 
    then () 
    else(
      let num = List.nth letters n in
      Graphics.draw_string num;
      Graphics.moveto ((obj_width * (n+3))-leeway1) ((obj_width*2)-15);
      draw_x (n+1)
    )
  in 
  let rec draw_y n =
    if n = world_size + 1
    then () 
    else if n < 10 
    then (
      let num = string_of_int n in
      Graphics.draw_string (" " ^ num);
      Graphics.moveto ((obj_width * 2)-15) ((obj_width * (n+2))-leeway2);
      draw_y (n+1)
    )
    else(
      let num = string_of_int n in
      Graphics.draw_string num;
      Graphics.moveto ((obj_width * 2)-15) ((obj_width * (n+2))-leeway2);
      draw_y (n+1)
    )
  in draw_x 0; 
  Graphics.moveto ((obj_width*2)-15) ((obj_width * 2)-leeway2);
  draw_y 1

(* Rounds a float to the nearest int *)
let roundfloat (f:float) : int = 
  let remainder = mod_float f 1. in
  if remainder < 0.5 then int_of_float f
  else (int_of_float f) + 1 

(** Finds the closest index that is next to the click **)
let round_click ((x,y):int*int) = 
  (abs (roundfloat ((float_of_int (x - (2*obj_width)))/.(float_of_int obj_width))), 
  abs (roundfloat ((float_of_int (y - (2*obj_width)))/.(float_of_int obj_width))))


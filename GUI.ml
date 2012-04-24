open Event
open Board
open Boardstuffs
(*open ImportImage*)

module GUI =
struct

  let mouse_state = ref false
  let mouse_pos = ref (0,0) 

  (** Events **)

  (** Fires when a key is pressed and returns the character corresponding
      to the key. *)
  let key_pressed : char Event.event = Event.new_event () 

  (** Fires when the mouse button is pressed, indicating the coordinates
      where the mouse was when the event occurred. *)
  let button_down : (int*int) Event.event = Event.new_event ()

  (** Fires when the mouse button is released, indicating the coordinates
      where the mouse was when the event occurred. *)
  let button_up : (int * int) Event.event = Event.new_event ()

  (** Handle events **)

  exception Stop

  (* Poll the Graphics module for the various events -- some care had to
     be taken to "de-bounce" the mouse. *)
  let read_event () = 
    let new_pos = Graphics.mouse_pos () in
    if Graphics.key_pressed () then begin
      Event.fire_event key_pressed (Graphics.read_key ())
    end ;
    if not !mouse_state then begin
      let s = Graphics.wait_next_event [Graphics.Button_down ; Graphics.Poll] in 
      if s.Graphics.button then begin
        mouse_state := true ; 
        Event.fire_event button_down new_pos
      end
    end ;
    if !mouse_state then begin
      let s = Graphics.wait_next_event [Graphics.Button_up ; Graphics.Poll] in 
      if not s.Graphics.button then begin
        mouse_state := false ; 
        Event.fire_event button_up new_pos
      end
    end 

  (** Read events and redraw the board **)
  let rec event_loop () = read_event () ; Graphics.synchronize () ; event_loop () 

  (** The command "run_ui x y init" starts up the graphical environment with a
      window size of x by y pixels, sets up the basic events such as the
      keyboard, mouse, etc. (see below), and then invokes the function init as
      an initializer, before entering an event polling loop which fires the
      appropriate event handlers whenever an action occurs. *)
  let run_ui (x:int) (y:int) (init:unit->unit) : unit = 
    try 
      Graphics.open_graph "" ; Graphics.resize_window x y ;
      Graphics.auto_synchronize false ; 
      init () ; 
      event_loop ()
    with exn -> (Graphics.close_graph () ; raise exn)

  (* Handle key presses *)
  let key_handler (press_handler : char -> unit) (c: char) =
    press_handler c

  (** Handle mouse clicks **)
  let mouse_handler (move_handler: int*int -> unit) (p:int*int) =
    move_handler p


  (** Start the graphical environment initialized to the size of the world.
      Handle clock and input events necessary to run the simulation. *)
  let run_game (init:unit -> unit) (* (handle_press :char -> unit) *)
    (handle_move:int*int -> unit) : unit =
    run_ui ((world_size+3)*obj_width) (* GUI width *)
           ((world_size+8)*obj_width) (* GUI height *)
           (* Event framework initializer *)
           begin fun () ->
             (* ignore(Event.add_listener key_pressed (key_handler handle_press)); *)
             ignore(Event.add_listener button_up (mouse_handler handle_move)) ;
             init ()
           end

end

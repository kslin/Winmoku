open Event

(** Dimensions **)

  (** The icon width for each position. *)
  let obj_width : int = 25

  (** The world has size x size positions *)
  let world_size : int = 5

type boardtype = Horizontal | Vertical | DiagLeft | DiagRight 

type index = int*int 

type threattype = StraightFour | Four | Three | SplitThree | WallThree

(* A threat has a gain square, cost squares, and rest squares *)
type threat = Threat of threattype * index * index list * index list 

type occupied = Black | White | Unocc 

let click_event : index Event.event = Event.new_event ()



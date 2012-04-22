open Event

(** Dimensions **)

  (** The icon width for each position. *)
  let obj_width : int = 25

  (** The world has size x size positions *)
  let world_size : int = 19

type boardtype = Horizontal | Vertical | DiagLeft | DiagRight 

type index = int*int 

type threattype = StraightFour | Four | Three | SplitThree | WallThree

(* A threat has a gain square, cost squares, and rest squares *)
type threat = Threat of threattype * index * index list * index list 

type occupied = Black | White | Unocc 

let click_event : index Event.event = Event.new_event ()

let print_index i = let (x,y) = i in
    print_string "(";
    print_int x;
    print_string ", ";
    print_int y;
    print_string ") ";
    flush_all () 

let rec print_index_list il = match il with
    |[] -> ()
    |hd::tl -> print_index hd;
        print_string "; ";
        print_index_list tl

let print_threats t = match t with
    |Threat(StraightFour,g,c,r) -> 
        print_string "Threat: Straight Four gain = ";
        print_index g;
        print_string ", cost = ";
        print_index_list c;
        print_string ", rest = ";
        print_index_list r;
        print_string "\n"
    |Threat(Four,g,c,r) ->
        print_string "Threat: Four gain = ";
        print_index g;
        print_string ", cost = ";
        print_index_list c;
        print_string ", rest = ";
        print_index_list r;
        print_string "\n"
    |Threat(Three,g,c,r) ->
        print_string "Threat: Three gain = ";
        print_index g;
        print_string ", cost = ";
        print_index_list c;
        print_string ", rest = ";
        print_index_list r;
        print_string "\n"
    |Threat(SplitThree,g,c,r) ->
        print_string "Threat: Split Three gain = ";
        print_index g;
        print_string ", cost = ";
        print_index_list c;
        print_string ", rest = ";
        print_index_list r;
        print_string "\n"
    |Threat(WallThree,g,c,r) ->
        print_string "Threat: Wall Three gain = ";
        print_index g;
        print_string ", cost = ";
        print_index_list c;
        print_string ", rest = ";
        print_index_list r;
        print_string "\n"

let rec print_threat_list tl = match tl with
    |[] -> ()
    |hd::tl -> print_threats hd;
        print_threat_list tl

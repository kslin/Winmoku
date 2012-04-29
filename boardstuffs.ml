exception ERROR

open Event

(** Dimensions **)

(** The icon width for each position. *)
let obj_width : int = 25

(** The world has size x size positions *)
let world_size : int = 19

(** The floor of the board **)
let floor : int = obj_width * 2

(** The ceiling of the board **)
let ceiling : int = (world_size + 1) * (obj_width)

(** Type for an index **)
type index = int*int 

(** The different types of threats **)
type threattype = Five | StraightFour | Four | Three | SplitThree | WallThree

(* A threat has a gain square, cost squares, and rest squares *)
type threat = Threat of threattype * index * index list * index list 

(** A space can be black, white, or unoccupied **)
type occupied = Black | White | Unocc 


(************************)
(*** Helper functions ***)
(************************)

(* Deoptionalize an option *)
let deopt x = match x with
        |None -> raise ERROR
        |Some s -> s

(* List of letters to be drawn *)
let letters = ["A";"B";"C";"D";"E";"F";"G";"H";"I";"J";
              "K";"L";"M";"N";"O";"P";"Q";"R";"S"]

(** Prints an index **)
let print_index i = let (x,y) = i in
    print_string "(";
    print_string (List.nth letters x);
    print_string ", ";
    print_int (y+1);
    print_string ") ";
    flush_all () 

(** Prints a list of indices **)
let rec print_index_list il = match il with
    |[] -> ()
    |hd::tl -> print_index hd;
        print_string "; ";
        print_index_list tl

(** Sorts a list of tuples **)
let tuple_sort (lst: index list) = 
        List.sort (fun (x1,y1) (x2,y2) -> y1 - y2) lst 

(* Create list of all tuples of the form (x,y) where x is from xs and
 * and y is from ys *)
let rec cross (xs:int list) (ys:int list) : (int*int) list = 
  match xs with
  | [] -> []
  | hd::tl -> List.map (fun y -> (hd,y)) ys @ cross tl ys

(* Generate a list of ints between n1 and n2 *)
let rec range (n1:int) (n2:int) : int list = 
  if n1 > n2 then [] else n1::(range (n1+1) n2)

(* Generate a list of indices within d distance of i *)
let rec indices_within (d: int) (i: index) = 
  let (x, y) = i in
  let xlow = max 0 (x-d) in
  let ylow = max 0 (y-d) in
  let xhigh = min (world_size-1) (x+d) in
  let yhigh = min (world_size-1) (y+d) in
    cross (range xlow xhigh) (range ylow yhigh)

(** Prints the color of a space **)
let print_occ c = match c with
        |Black -> print_string " Black "; flush_all ()
        |White -> print_string " White "; flush_all ()
        |Unocc -> print_string " Unocc "; flush_all ()

(** Prints threats **)
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
    |Threat(Five,g,c,r) ->
        print_string "Threat: Five gain = ";
        print_index g ;
	    print_string ", cost = ";
	    print_index_list c;
	    print_string ", rest = ";
	    print_index_list r;
	    print_string "\n"

(** Prints a list of threats **)
let rec print_threat_list tl = match tl with
    |[] -> ()
    |hd::tl -> print_threats hd;
        print_threat_list tl

(** Gets string form of index **)
let get_string_i i = let (x,y) = i in
    "(" ^
    (List.nth letters x) ^
    ", " ^
    (string_of_int (y+1)) ^
    ") "

(** Prints gains **)
let gain_string t = match t with
    |Threat(StraightFour,g,c,r) -> 
        "Threat: Straight Four gain = " ^
        (get_string_i g) ^
        "\n"
    |Threat(Four,g,c,r) ->
        "Threat: Four gain = " ^
        (get_string_i g) ^
        "\n"
    |Threat(Three,g,c,r) ->
        "Threat: Three gain = " ^
        (get_string_i g) ^
        "\n"
    |Threat(SplitThree,g,c,r) ->
        "Threat: Split Three gain = " ^
        (get_string_i g) ^
        "\n"
    |Threat(WallThree,g,c,r) ->
        "Threat: Wall Three gain = " ^
        (get_string_i g) ^
        "\n"
    |Threat(Five,g,c,r) ->
        "Threat: Five gain = " ^
        (get_string_i g) ^
        "\n"

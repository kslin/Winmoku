open Graphics

type dot = White | Black | None

class type board_object_i = 
object
	(** the size/dim of the board **)
	val size : int

	(** the position for each position **) 	
	val index : int * int

	(** the status of each index such as white, black, or none **)
	val dot : dot

	(** the board as a list of index **)
	val board : (index * dot) list

	(** draw lines on board **)
	method draw_grid : unit

	(** draw dots on board **)
	method draw_dots : unit

	(** returns position of the mouse **)
	method mouse_pos : unit

	(**  **)



end


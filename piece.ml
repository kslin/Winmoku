open Boardstuffs

class type piece_object = 
object

	val mutable value: occupied

	method get_value : occupied

	method set_value : occupied -> unit

	method get_neighbors : index list list

	method set_h_neighbors : index list -> unit

	method set_v_neighbors : index list -> unit

	method set_dl_neighbors : index list -> unit

	method set_dr_neighbors : index list -> unit

end

class piece =
object

	val mutable value: occupied = Unocc
	val mutable neighbors: index list list = []
	val mutable h_neighbors: index list = []
	val mutable v_neighbors: index list = []
	val mutable dl_neighbors: index list = []
	val mutable dr_neighbors: index list = []

	method get_value = value

	method set_value newval = value <- newval

	method get_neighbors : index list list = neighbors

	method set_h_neighbors (i: index list) = h_neighbors <- i

	method set_v_neighbors (i: index list) = v_neighbors <- i

	method set_dl_neighbors (i: index list) = dl_neighbors <- i

	method set_dr_neighbors (i: index list) = dr_neighbors <- i

end
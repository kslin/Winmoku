open Boardstuffs

class type piece_object = 
object

	val mutable value: occupied

	method get_value : occupied

	method set_value : occupied -> unit

	method draw : index -> unit

end
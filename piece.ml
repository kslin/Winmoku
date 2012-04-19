open Boardstuffs

class type piece_object = 
object

	val mutable value: occupied

	method get_value : occupied

	method set_value : occupied -> unit

end

class piece (v: occupied): piece_object =
object

	val mutable value: occupied = v

	method get_value = value

	method set_value newval = value <- newval

end
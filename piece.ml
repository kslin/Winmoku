open Boardstuffs
open Pieceobject
open Graphics

class piece (v: occupied): piece_object =
object (self)

	val mutable value: occupied = v

	method get_value = value

	method set_value newval = value <- newval

	method draw (i:index) = match v with
		|Black -> self#circle i obj_width obj_height Graphics.black
		|White -> self#circle i obj_width obj_height Graphics.white
		|Unocc -> self#cross i obj_width obj_height

	method private circle ((x,y):int*int) (width:int) (height:int)
             (bg:Graphics.color) : unit =
    	Graphics.set_color bg ;
    	Graphics.fill_circle (x*width) (y*height) 
                         (min width height / 2) 

    method private cross ((x,y):int*int) (width:int) (height:int) : unit = 
    	Graphics.set_color Graphics.black;
    	Graphics.fill_rect (x*width - (width/2)) (y*height) width (height/10);
    	Graphics.fill_rect (x*width) (y*height - (height/2)) (width/10) (height)

end
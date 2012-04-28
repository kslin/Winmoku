open Board
open Boardstuffs

let run_all () = 
	print_threat_list (Myboard.getThreats t_d_board)

let _ = run_all ()
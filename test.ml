open Main

let t_d_board = 
	Myboard.empty

let run_all () = 
	print_threat_list (My.board getThreats t_d_board)

let _ = run_all ()
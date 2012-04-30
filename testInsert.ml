open Board
open Boardstuffs
open Threats

(* keeps track of successful tests *)
let successful : int ref = ref 0

let run_tests_insertblack () =
	let bor = Myboard.empty in
	let rec insert_row x y = 
		if y = world_size 
		then () 
		else (
			assert (Myboard.get bor (x,y) = Unocc);
			let bor = Myboard.insertspecial bor (x,y) Black in
			assert (Myboard.get bor (x,y) = Black);
			successful := !successful + 1;
			insert_row x (y+1))
	in let rec insert_rows x = 
		if x = world_size
		then ()
		else (
			insert_row x 0;
			insert_rows (x+1) )
	in insert_rows 0

let run_tests_insertwhite () =
	let bor = Myboard.empty in
	let rec insert_row x y = 
		if y = world_size 
		then () 
		else (
			assert (Myboard.get bor (x,y) = Unocc);
			let bor = Myboard.insertspecial bor (x,y) White in
			assert (Myboard.get bor (x,y) = White);
			successful := !successful + 1;
			insert_row x (y+1))
	in let rec insert_rows x = 
		if x = world_size
		then ()
		else (
			insert_row x 0;
			insert_rows (x+1) )
	in insert_rows 0

let _ = run_tests_insertblack ();
		run_tests_insertwhite ();
		print_string "\ninsertspecial successfully passed: ";
		print_int !successful;
		print_string "/722 tests.\n\n";
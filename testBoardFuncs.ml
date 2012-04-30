open Board
open Boardstuffs
open Threats

(* keeps track of successful tests *)
let successful1 : int ref = ref 0
let successful2 : int ref = ref 0

let run_tests_isWinBlackH () =
	let bor = Myboard.empty in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (3,4) Black in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (4,4) Black in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (5,4) Black in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (6,4) Black in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (7,4) Black in
	assert (Myboard.isWin bor = Some Black);
	successful1 := !successful1 + 1

let run_tests_isWinBlackV () =
	let bor = Myboard.empty in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (9,3) Black in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (9,4) Black in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (9,5) Black in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (9,6) Black in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (9,7) Black in
	assert (Myboard.isWin bor = Some Black);
	successful1 := !successful1 + 1

let run_tests_isWinBlackDR () =
	let bor = Myboard.empty in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (10,4) Black in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (9,5) Black in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (8,6) Black in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (7,7) Black in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (6,8) Black in
	assert (Myboard.isWin bor = Some Black);
	successful1 := !successful1 + 1

let run_tests_isWinBlackDL () =
	let bor = Myboard.empty in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (3,9) Black in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (4,10) Black in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (5,11) Black in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (6,12) Black in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (7,13) Black in
	assert (Myboard.isWin bor = Some Black);
	successful1 := !successful1 + 1

let run_tests_isWinWhiteH () =
	let bor = Myboard.empty in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (3,4) White in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (4,4) White in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (5,4) White in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (6,4) White in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (7,4) White in
	assert (Myboard.isWin bor = Some White);
	successful1 := !successful1 + 1

let run_tests_isWinWhiteV () =
	let bor = Myboard.empty in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (9,3) White in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (9,4) White in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (9,5) White in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (9,6) White in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (9,7) White in
	assert (Myboard.isWin bor = Some White);
	successful1 := !successful1 + 1

let run_tests_isWinWhiteDR () =
	let bor = Myboard.empty in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (10,4) White in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (9,5) White in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (8,6) White in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (7,7) White in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (6,8) White in
	assert (Myboard.isWin bor = Some White);
	successful1 := !successful1 + 1

let run_tests_isWinWhiteDL () =
	let bor = Myboard.empty in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (3,9) White in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (4,10) White in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (5,11) White in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (6,12) White in
	assert (Myboard.isWin bor = None);
	successful1 := !successful1 + 1;
	let bor = Myboard.insertspecial bor (7,13) White in
	assert (Myboard.isWin bor = Some White);
	successful1 := !successful1 + 1

let _ = run_tests_isWinBlackH ();
		run_tests_isWinBlackV ();
		run_tests_isWinBlackDR ();
		run_tests_isWinBlackDL ();
		run_tests_isWinWhiteH ();
		run_tests_isWinWhiteV ();
		run_tests_isWinWhiteDR ();
		run_tests_isWinWhiteDL ();
		print_string "\nisWin successfully passed: ";
		print_int !successful1;
		print_string "/48 tests.\n\n";





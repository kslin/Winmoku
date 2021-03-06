open Board
open Boardstuffs
open Threats

(* There are 20 different kinds of scenarios that would lead to a threat 
	These can occur in all 4 directions 
	80 scenarios were tested total *)
let successful : int ref = ref 0

let run_tests_horizontal () =
	let bor = Myboard.empty in
	let bor1 = Myboard.insertspecial bor (0,7) Black in
		let bor1 = Myboard.insertspecial bor1 (1,7) Black in 
		let bor1 = Myboard.insertspecial bor1 (2,7) Black in
	let bor2 = Myboard.insertspecial bor (1,7) Black in
		let bor2 = Myboard.insertspecial bor2 (2,7) Black in 
		let bor2 = Myboard.insertspecial bor2 (3,7) Black in
		let bor2 = Myboard.insertspecial bor2 (5,7) White in
	let bor3 = Myboard.insertspecial bor (1,7) Black in
		let bor3 = Myboard.insertspecial bor3 (2,7) Black in 
		let bor3 = Myboard.insertspecial bor3 (3,7) Black in
	let bor4 = Myboard.insertspecial bor (16,7) Black in
		let bor4 = Myboard.insertspecial bor4 (17,7) Black in 
		let bor4 = Myboard.insertspecial bor4 (18,7) Black in
	let bor5 = Myboard.insertspecial bor (15,7) Black in
		let bor5 = Myboard.insertspecial bor5 (16,7) Black in 
		let bor5 = Myboard.insertspecial bor5 (17,7) Black in
	let bor6 = Myboard.insertspecial bor (10,7) Black in
		let bor6 = Myboard.insertspecial bor6 (11,7) Black in 
		let bor6 = Myboard.insertspecial bor6 (12,7) Black in
	let bor7 = Myboard.insertspecial bor (0,7) Black in
		let bor7 = Myboard.insertspecial bor7 (1,7) Black in 
		let bor7 = Myboard.insertspecial bor7 (3,7) Black in
	let bor8 = Myboard.insertspecial bor (15,7) Black in
		let bor8 = Myboard.insertspecial bor8 (17,7) Black in 
		let bor8 = Myboard.insertspecial bor8 (18,7) Black in
	let bor9 = Myboard.insertspecial bor (0,7) Black in
		let bor9 = Myboard.insertspecial bor9 (2,7) Black in 
		let bor9 = Myboard.insertspecial bor9 (3,7) Black in
	let bor10 = Myboard.insertspecial bor (15,7) Black in
		let bor10 = Myboard.insertspecial bor10 (16,7) Black in 
		let bor10 = Myboard.insertspecial bor10 (18,7) Black in
	let bor11 = Myboard.insertspecial bor (4,12) Black in
		let bor11 = Myboard.insertspecial bor11 (5,12) Black in 
		let bor11 = Myboard.insertspecial bor11 (7,12) Black in
	let bor12 = Myboard.insertspecial bor (6,12) Black in
		let bor12 = Myboard.insertspecial bor12 (8,12) Black in 
		let bor12 = Myboard.insertspecial bor12 (9,12) Black in
	let bor13 = Myboard.insertspecial bor (6,12) White in
		let bor13 = Myboard.insertspecial bor13 (8,12) Black in 
		let bor13 = Myboard.insertspecial bor13 (9,12) Black in
		let bor13 = Myboard.insertspecial bor13 (12,12) White in
	let bor14 = Myboard.insertspecial bor (6,12) White in
		let bor14 = Myboard.insertspecial bor14 (9,12) Black in 
		let bor14 = Myboard.insertspecial bor14 (10,12) Black in
		let bor14 = Myboard.insertspecial bor14 (12,12) White in
	let bor15 = Myboard.insertspecial bor (6,12) White in
		let bor15 = Myboard.insertspecial bor15 (8,12) Black in 
		let bor15 = Myboard.insertspecial bor15 (9,12) Black in
	let bor16 = Myboard.insertspecial bor (9,12) Black in 
		let bor16 = Myboard.insertspecial bor16 (10,12) Black in
		let bor16 = Myboard.insertspecial bor16 (12,12) White in
	let bor17 = Myboard.insertspecial bor (6,12) White in
		let bor17 = Myboard.insertspecial bor17 (9,12) Black in 
		let bor17 = Myboard.insertspecial bor17 (10,12) Black in
		let bor17 = Myboard.insertspecial bor17 (13,12) White in
	let bor18 = Myboard.insertspecial bor (6,12) White in
		let bor18 = Myboard.insertspecial bor18 (9,12) Black in 
		let bor18 = Myboard.insertspecial bor18 (10,12) Black in
	let bor19 = Myboard.insertspecial bor (9,12) Black in 
		let bor19 = Myboard.insertspecial bor19 (10,12) Black in
		let bor19 = Myboard.insertspecial bor19 (13,12) White in
	let bor20 = Myboard.insertspecial bor (9,12) Black in 
		let bor20 = Myboard.insertspecial bor20 (10,12) Black in
	let bor21 = Myboard.insertspecial bor (4,12) Black in
		let bor21 = Myboard.insertspecial bor21 (5,12) Black in 
		let bor21 = Myboard.insertspecial bor21 (8,12) Black in
	let bor22 = Myboard.insertspecial bor (4,12) Black in
		let bor22 = Myboard.insertspecial bor22 (7,12) Black in 
		let bor22 = Myboard.insertspecial bor22 (8,12) Black in
	let bor23 = Myboard.insertspecial bor (4,12) White in
		let bor23 = Myboard.insertspecial bor23 (6,12) Black in 
		let bor23 = Myboard.insertspecial bor23 (8,12) Black in
	let bor24 = Myboard.insertspecial bor (4,12) Black in
		let bor24 = Myboard.insertspecial bor24 (6,12) Black in 
		let bor24 = Myboard.insertspecial bor24 (8,12) White in
	let bor25 = Myboard.insertspecial bor (4,12) Black in
		let bor25 = Myboard.insertspecial bor25 (6,12) Black in 
	let bor26 = Myboard.insertspecial bor (4,12) Black in
		let bor26 = Myboard.insertspecial bor26 (6,12) Black in 
		let bor26 = Myboard.insertspecial bor26 (8,12) Black in



	assert (List.length (Myboard.getThreats bor1) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor2) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor3) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor4) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor5) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor6) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor7) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor8) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor9) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor10) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor11) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor12) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor13) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor14) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor15) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor16) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor17) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor18) = 3);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor19) = 3);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor20) = 4);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor21) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor22) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor23) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor24) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor25) = 3);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor26) = 2);
	successful := !successful + 1

let run_tests_vertical () =
	let bor = Myboard.empty in
	let bor1 = Myboard.insertspecial bor (11,0) Black in
		let bor1 = Myboard.insertspecial bor1 (11,1) Black in 
		let bor1 = Myboard.insertspecial bor1 (11,2) Black in
	let bor2 = Myboard.insertspecial bor (11,1) Black in
		let bor2 = Myboard.insertspecial bor2 (11,2) Black in 
		let bor2 = Myboard.insertspecial bor2 (11,3) Black in
		let bor2 = Myboard.insertspecial bor2 (11,5) White in
	let bor3 = Myboard.insertspecial bor (11,1) Black in
		let bor3 = Myboard.insertspecial bor3 (11,2) Black in 
		let bor3 = Myboard.insertspecial bor3 (11,3) Black in
	let bor4 = Myboard.insertspecial bor (11,16) Black in
		let bor4 = Myboard.insertspecial bor4 (11,17) Black in 
		let bor4 = Myboard.insertspecial bor4 (11,18) Black in
	let bor5 = Myboard.insertspecial bor (11,15) Black in
		let bor5 = Myboard.insertspecial bor5 (11,16) Black in 
		let bor5 = Myboard.insertspecial bor5 (11,17) Black in
	let bor6 = Myboard.insertspecial bor (11,10) Black in
		let bor6 = Myboard.insertspecial bor6 (11,11) Black in 
		let bor6 = Myboard.insertspecial bor6 (11,12) Black in
	let bor7 = Myboard.insertspecial bor (11,0) Black in
		let bor7 = Myboard.insertspecial bor7 (11,1) Black in 
		let bor7 = Myboard.insertspecial bor7 (11,3) Black in
	let bor8 = Myboard.insertspecial bor (11,15) Black in
		let bor8 = Myboard.insertspecial bor8 (11,17) Black in 
		let bor8 = Myboard.insertspecial bor8 (11,18) Black in
	let bor9 = Myboard.insertspecial bor (11,0) Black in
		let bor9 = Myboard.insertspecial bor9 (11,2) Black in 
		let bor9 = Myboard.insertspecial bor9 (11,3) Black in
	let bor10 = Myboard.insertspecial bor (11,15) Black in
		let bor10 = Myboard.insertspecial bor10 (11,16) Black in 
		let bor10 = Myboard.insertspecial bor10 (11,18) Black in
	let bor11 = Myboard.insertspecial bor (5,4) Black in
		let bor11 = Myboard.insertspecial bor11 (5,5) Black in 
		let bor11 = Myboard.insertspecial bor11 (5,7) Black in
	let bor12 = Myboard.insertspecial bor (5,6) Black in
		let bor12 = Myboard.insertspecial bor12 (5,8) Black in 
		let bor12 = Myboard.insertspecial bor12 (5,9) Black in
	let bor13 = Myboard.insertspecial bor (5,6) White in
		let bor13 = Myboard.insertspecial bor13 (5,8) Black in 
		let bor13 = Myboard.insertspecial bor13 (5,9) Black in
		let bor13 = Myboard.insertspecial bor13 (5,12) White in
	let bor14 = Myboard.insertspecial bor (5,6) White in
		let bor14 = Myboard.insertspecial bor14 (5,9) Black in 
		let bor14 = Myboard.insertspecial bor14 (5,10) Black in
		let bor14 = Myboard.insertspecial bor14 (5,12) White in
	let bor15 = Myboard.insertspecial bor (5,6) White in
		let bor15 = Myboard.insertspecial bor15 (5,8) Black in 
		let bor15 = Myboard.insertspecial bor15 (5,9) Black in
	let bor16 = Myboard.insertspecial bor (5,9) Black in 
		let bor16 = Myboard.insertspecial bor16 (5,10) Black in
		let bor16 = Myboard.insertspecial bor16 (5,12) White in
	let bor17 = Myboard.insertspecial bor (5,6) White in
		let bor17 = Myboard.insertspecial bor17 (5,9) Black in 
		let bor17 = Myboard.insertspecial bor17 (5,10) Black in
		let bor17 = Myboard.insertspecial bor17 (5,13) White in
	let bor18 = Myboard.insertspecial bor (5,6) White in
		let bor18 = Myboard.insertspecial bor18 (5,9) Black in 
		let bor18 = Myboard.insertspecial bor18 (5,10) Black in
	let bor19 = Myboard.insertspecial bor (5,9) Black in 
		let bor19 = Myboard.insertspecial bor19 (5,10) Black in
		let bor19 = Myboard.insertspecial bor19 (5,13) White in
	let bor20 = Myboard.insertspecial bor (5,9) Black in 
		let bor20 = Myboard.insertspecial bor20 (5,10) Black in
	let bor21 = Myboard.insertspecial bor (12,4) Black in
		let bor21 = Myboard.insertspecial bor21 (12,5) Black in 
		let bor21 = Myboard.insertspecial bor21 (12,8) Black in
	let bor22 = Myboard.insertspecial bor (12,4) Black in
		let bor22 = Myboard.insertspecial bor22 (12,7) Black in 
		let bor22 = Myboard.insertspecial bor22 (12,8) Black in
	let bor23 = Myboard.insertspecial bor (12,4) White in
		let bor23 = Myboard.insertspecial bor23 (12,6) Black in 
		let bor23 = Myboard.insertspecial bor23 (12,8) Black in
	let bor24 = Myboard.insertspecial bor (12,4) Black in
		let bor24 = Myboard.insertspecial bor24 (12,6) Black in 
		let bor24 = Myboard.insertspecial bor24 (12,8) White in
	let bor25 = Myboard.insertspecial bor (12,4) Black in
		let bor25 = Myboard.insertspecial bor25 (12,6) Black in 
	let bor26 = Myboard.insertspecial bor (12,4) Black in
		let bor26 = Myboard.insertspecial bor26 (12,6) Black in 
		let bor26 = Myboard.insertspecial bor26 (12,8) Black in



	assert (List.length (Myboard.getThreats bor1) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor2) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor3) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor4) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor5) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor6) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor7) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor8) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor9) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor10) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor11) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor12) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor13) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor14) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor15) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor16) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor17) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor18) = 3);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor19) = 3);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor20) = 4);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor21) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor22) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor23) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor24) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor25) = 3);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor26) = 2);
	successful := !successful + 1


let run_tests_diagright () =
	let bor = Myboard.empty in
	let bor1 = Myboard.insertspecial bor (0,12) Black in
		let bor1 = Myboard.insertspecial bor1 (1,11) Black in 
		let bor1 = Myboard.insertspecial bor1 (2,10) Black in
	let bor2 = Myboard.insertspecial bor (1,10) Black in
		let bor2 = Myboard.insertspecial bor2 (2,9) Black in 
		let bor2 = Myboard.insertspecial bor2 (3,8) Black in
		let bor2 = Myboard.insertspecial bor2 (5,6) White in
	let bor3 = Myboard.insertspecial bor (1,10) Black in
		let bor3 = Myboard.insertspecial bor3 (2,9) Black in 
		let bor3 = Myboard.insertspecial bor3 (3,8) Black in
	let bor4 = Myboard.insertspecial bor (11,2) Black in
		let bor4 = Myboard.insertspecial bor4 (12,1) Black in 
		let bor4 = Myboard.insertspecial bor4 (13,0) Black in
	let bor5 = Myboard.insertspecial bor (10,3) Black in
		let bor5 = Myboard.insertspecial bor5 (11,2) Black in 
		let bor5 = Myboard.insertspecial bor5 (12,1) Black in
	let bor6 = Myboard.insertspecial bor (7,10) Black in
		let bor6 = Myboard.insertspecial bor6 (8,9) Black in 
		let bor6 = Myboard.insertspecial bor6 (9,8) Black in
	let bor7 = Myboard.insertspecial bor (0,12) Black in
		let bor7 = Myboard.insertspecial bor7 (1,11) Black in 
		let bor7 = Myboard.insertspecial bor7 (3,9) Black in
	let bor8 = Myboard.insertspecial bor (7,3) Black in
		let bor8 = Myboard.insertspecial bor8 (9,1) Black in 
		let bor8 = Myboard.insertspecial bor8 (10,0) Black in
	let bor9 = Myboard.insertspecial bor (0,12) Black in
		let bor9 = Myboard.insertspecial bor9 (2,10) Black in 
		let bor9 = Myboard.insertspecial bor9 (3,9) Black in
	let bor10 = Myboard.insertspecial bor (7,3) Black in
		let bor10 = Myboard.insertspecial bor10 (8,2) Black in 
		let bor10 = Myboard.insertspecial bor10 (10,0) Black in
	let bor11 = Myboard.insertspecial bor (8,6) Black in
		let bor11 = Myboard.insertspecial bor11 (7,5) Black in 
		let bor11 = Myboard.insertspecial bor11 (5,3) Black in
	let bor12 = Myboard.insertspecial bor (8,6) Black in
		let bor12 = Myboard.insertspecial bor12 (6,4) Black in 
		let bor12 = Myboard.insertspecial bor12 (5,3) Black in
	let bor13 = Myboard.insertspecial bor (4,11) White in
		let bor13 = Myboard.insertspecial bor13 (6,9) Black in 
		let bor13 = Myboard.insertspecial bor13 (7,8) Black in
		let bor13 = Myboard.insertspecial bor13 (10,5) White in
	let bor14 = Myboard.insertspecial bor (4,11) White in
		let bor14 = Myboard.insertspecial bor14 (7,8) Black in 
		let bor14 = Myboard.insertspecial bor14 (8,7) Black in
		let bor14 = Myboard.insertspecial bor14 (10,5) White in
	let bor15 = Myboard.insertspecial bor (4,11) White in
		let bor15 = Myboard.insertspecial bor15 (6,9) Black in 
		let bor15 = Myboard.insertspecial bor15 (7,8) Black in
	let bor16 = Myboard.insertspecial bor (6,9) Black in 
		let bor16 = Myboard.insertspecial bor16 (7,8) Black in
		let bor16 = Myboard.insertspecial bor16 (9,6) White in
	let bor17 = Myboard.insertspecial bor (5,10) White in
		let bor17 = Myboard.insertspecial bor17 (8,7) Black in 
		let bor17 = Myboard.insertspecial bor17 (9,6) Black in
		let bor17 = Myboard.insertspecial bor17 (12,3) White in
	let bor18 = Myboard.insertspecial bor (5,10) White in
		let bor18 = Myboard.insertspecial bor18 (8,7) Black in 
		let bor18 = Myboard.insertspecial bor18 (9,6) Black in
	let bor19 = Myboard.insertspecial bor (8,7) Black in 
		let bor19 = Myboard.insertspecial bor19 (9,6) Black in
		let bor19 = Myboard.insertspecial bor19 (12,3) White in
	let bor20 = Myboard.insertspecial bor (8,7) Black in 
		let bor20 = Myboard.insertspecial bor20 (9,6) Black in
	let bor21 = Myboard.insertspecial bor (7,12) Black in
		let bor21 = Myboard.insertspecial bor21 (8,11) Black in 
		let bor21 = Myboard.insertspecial bor21 (11,8) Black in
	let bor22 = Myboard.insertspecial bor (7,12) Black in
		let bor22 = Myboard.insertspecial bor22 (10,9) Black in 
		let bor22 = Myboard.insertspecial bor22 (11,8) Black in
	let bor23 = Myboard.insertspecial bor (7,12) White in
		let bor23 = Myboard.insertspecial bor23 (9,10) Black in 
		let bor23 = Myboard.insertspecial bor23 (11,8) Black in
	let bor24 = Myboard.insertspecial bor (7,12) Black in
		let bor24 = Myboard.insertspecial bor24 (9,10) Black in 
		let bor24 = Myboard.insertspecial bor24 (11,8) White in
	let bor25 = Myboard.insertspecial bor (7,12) Black in
		let bor25 = Myboard.insertspecial bor25 (9,10) Black in 
	let bor26 = Myboard.insertspecial bor (7,10) Black in
		let bor26 = Myboard.insertspecial bor26 (9,8) Black in 
		let bor26 = Myboard.insertspecial bor26 (11,6) Black in



	assert (List.length (Myboard.getThreats bor1) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor2) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor3) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor4) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor5) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor6) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor7) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor8) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor9) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor10) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor11) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor12) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor13) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor14) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor15) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor16) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor17) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor18) = 3);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor19) = 3);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor20) = 4);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor21) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor22) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor23) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor24) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor25) = 3);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor26) = 2);
	successful := !successful + 1

let run_tests_diagleft () =
	let bor = Myboard.empty in
	let bor1 = Myboard.insertspecial bor (0,7) Black in
		let bor1 = Myboard.insertspecial bor1 (1,8) Black in 
		let bor1 = Myboard.insertspecial bor1 (2,9) Black in
	let bor2 = Myboard.insertspecial bor (1,7) Black in
		let bor2 = Myboard.insertspecial bor2 (2,8) Black in 
		let bor2 = Myboard.insertspecial bor2 (3,9) Black in
		let bor2 = Myboard.insertspecial bor2 (5,11) White in
	let bor3 = Myboard.insertspecial bor (1,7) Black in
		let bor3 = Myboard.insertspecial bor3 (2,8) Black in 
		let bor3 = Myboard.insertspecial bor3 (3,9) Black in
	let bor4 = Myboard.insertspecial bor (16,7) Black in
		let bor4 = Myboard.insertspecial bor4 (17,8) Black in 
		let bor4 = Myboard.insertspecial bor4 (18,9) Black in
	let bor5 = Myboard.insertspecial bor (15,7) Black in
		let bor5 = Myboard.insertspecial bor5 (16,8) Black in 
		let bor5 = Myboard.insertspecial bor5 (17,9) Black in
	let bor6 = Myboard.insertspecial bor (10,7) Black in
		let bor6 = Myboard.insertspecial bor6 (11,8) Black in 
		let bor6 = Myboard.insertspecial bor6 (12,9) Black in
	let bor7 = Myboard.insertspecial bor (0,7) Black in
		let bor7 = Myboard.insertspecial bor7 (1,8) Black in 
		let bor7 = Myboard.insertspecial bor7 (3,10) Black in
	let bor8 = Myboard.insertspecial bor (15,7) Black in
		let bor8 = Myboard.insertspecial bor8 (17,9) Black in 
		let bor8 = Myboard.insertspecial bor8 (18,10) Black in
	let bor9 = Myboard.insertspecial bor (0,7) Black in
		let bor9 = Myboard.insertspecial bor9 (2,9) Black in 
		let bor9 = Myboard.insertspecial bor9 (3,10) Black in
	let bor10 = Myboard.insertspecial bor (15,7) Black in
		let bor10 = Myboard.insertspecial bor10 (16,8) Black in 
		let bor10 = Myboard.insertspecial bor10 (18,10) Black in
	let bor11 = Myboard.insertspecial bor (4,12) Black in
		let bor11 = Myboard.insertspecial bor11 (5,13) Black in 
		let bor11 = Myboard.insertspecial bor11 (7,15) Black in
	let bor12 = Myboard.insertspecial bor (6,12) Black in
		let bor12 = Myboard.insertspecial bor12 (8,14) Black in 
		let bor12 = Myboard.insertspecial bor12 (9,15) Black in
	let bor13 = Myboard.insertspecial bor (6,12) White in
		let bor13 = Myboard.insertspecial bor13 (8,14) Black in 
		let bor13 = Myboard.insertspecial bor13 (9,15) Black in
		let bor13 = Myboard.insertspecial bor13 (12,18) White in
	let bor14 = Myboard.insertspecial bor (6,12) White in
		let bor14 = Myboard.insertspecial bor14 (9,15) Black in 
		let bor14 = Myboard.insertspecial bor14 (10,16) Black in
		let bor14 = Myboard.insertspecial bor14 (12,18) White in
	let bor15 = Myboard.insertspecial bor (6,8) White in
		let bor15 = Myboard.insertspecial bor15 (8,10) Black in 
		let bor15 = Myboard.insertspecial bor15 (9,11) Black in
	let bor16 = Myboard.insertspecial bor (9,9) Black in 
		let bor16 = Myboard.insertspecial bor16 (10,10) Black in
		let bor16 = Myboard.insertspecial bor16 (12,12) White in
	let bor17 = Myboard.insertspecial bor (6,10) White in
		let bor17 = Myboard.insertspecial bor17 (9,13) Black in 
		let bor17 = Myboard.insertspecial bor17 (10,14) Black in
		let bor17 = Myboard.insertspecial bor17 (13,17) White in
	let bor18 = Myboard.insertspecial bor (6,10) White in
		let bor18 = Myboard.insertspecial bor18 (9,13) Black in 
		let bor18 = Myboard.insertspecial bor18 (10,14) Black in
	let bor19 = Myboard.insertspecial bor (9,13) Black in 
		let bor19 = Myboard.insertspecial bor19 (10,14) Black in
		let bor19 = Myboard.insertspecial bor19 (13,17) White in
	let bor20 = Myboard.insertspecial bor (9,12) Black in 
		let bor20 = Myboard.insertspecial bor20 (10,13) Black in
	let bor21 = Myboard.insertspecial bor (4,8) Black in
		let bor21 = Myboard.insertspecial bor21 (5,9) Black in 
		let bor21 = Myboard.insertspecial bor21 (8,12) Black in
	let bor22 = Myboard.insertspecial bor (4,8) Black in
		let bor22 = Myboard.insertspecial bor22 (7,11) Black in 
		let bor22 = Myboard.insertspecial bor22 (8,12) Black in
	let bor23 = Myboard.insertspecial bor (4,8) White in
		let bor23 = Myboard.insertspecial bor23 (6,10) Black in 
		let bor23 = Myboard.insertspecial bor23 (8,12) Black in
	let bor24 = Myboard.insertspecial bor (4,8) Black in
		let bor24 = Myboard.insertspecial bor24 (6,10) Black in 
		let bor24 = Myboard.insertspecial bor24 (8,12) White in
	let bor25 = Myboard.insertspecial bor (4,10) Black in
		let bor25 = Myboard.insertspecial bor25 (6,12) Black in 
	let bor26 = Myboard.insertspecial bor (4,8) Black in
		let bor26 = Myboard.insertspecial bor26 (6,10) Black in 
		let bor26 = Myboard.insertspecial bor26 (8,12) Black in



	assert (List.length (Myboard.getThreats bor1) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor2) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor3) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor4) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor5) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor6) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor7) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor8) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor9) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor10) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor11) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor12) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor13) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor14) = 1);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor15) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor16) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor17) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor18) = 3);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor19) = 3);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor20) = 4);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor21) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor22) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor23) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor24) = 2);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor25) = 3);
	successful := !successful + 1;
	assert (List.length (Myboard.getThreats bor26) = 2);
	successful := !successful + 1

let _ = run_tests_horizontal ();
		run_tests_vertical ();
		run_tests_diagright ();
		run_tests_diagleft ();
		print_string "\ngetThreats successfully passed: ";
		print_int !successful;
		print_string "/104 tests.\n\n";

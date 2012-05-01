open Board
open Boardstuffs
open Threats

let successful : int ref = ref 0

(* paper with good boards to test threats:
   http://library.thinkquest.org/18242/data/resources/gomoku.pdf *)

let test_evaluate_board () = 
  (* board in figure 3a of paper has many potential threats but
     no winning sequence *)
  let b1 = Myboard.empty in
  let b1 = Myboard.insertspecial b1 (0,0) Black in
  let b1 = Myboard.insertspecial b1 (0,1) Black in
  let b1 = Myboard.insertspecial b1 (0,2) Black in
  let b1 = Myboard.insertspecial b1 (1,0) Black in
  let b1 = Myboard.insertspecial b1 (2,0) Black in
  let b1 = Myboard.insertspecial b1 (18,18) Black in
  let b1 = Myboard.insertspecial b1 (18,17) Black in
  let b1 = Myboard.insertspecial b1 (18,16) Black in
  let b1 = Myboard.insertspecial b1 (16,18) Black in
  let b1 = Myboard.insertspecial b1 (17,18) Black in
  let b1 = Myboard.insertspecial b1 (18,0) Black in
  let b1 = Myboard.insertspecial b1 (18,1) Black in
  let b1 = Myboard.insertspecial b1 (18,2) Black in
  let b1 = Myboard.insertspecial b1 (16,18) Black in
  let b1 = Myboard.insertspecial b1 (17,18) Black in
  let b1 = Myboard.insertspecial b1 (0,18) Black in
  let b1 = Myboard.insertspecial b1 (0,17) Black in
  let b1 = Myboard.insertspecial b1 (0,16) Black in
  let b1 = Myboard.insertspecial b1 (1,18) Black in
  let b1 = Myboard.insertspecial b1 (2,18) Black in
  assert(BThreats.evaluate_board b1 = None) ;
  successful := !successful + 1 ;

  (* board in figure 3b of paper that has a winning sequence *)
  let b2 = Myboard.empty in
  let b2 = Myboard.insertspecial b2 (0,0) Black in
  let b2 = Myboard.insertspecial b2 (0,1) Black in
  let b2 = Myboard.insertspecial b2 (0,2) Black in
  let b2 = Myboard.insertspecial b2 (1,0) Black in
  let b2 = Myboard.insertspecial b2 (2,0) Black in
  let b2 = Myboard.insertspecial b2 (18,18) Black in
  let b2 = Myboard.insertspecial b2 (18,17) Black in
  let b2 = Myboard.insertspecial b2 (18,16) Black in
  let b2 = Myboard.insertspecial b2 (16,18) Black in
  let b2 = Myboard.insertspecial b2 (17,18) Black in
  let b2 = Myboard.insertspecial b2 (18,0) Black in
  let b2 = Myboard.insertspecial b2 (18,1) Black in
  let b2 = Myboard.insertspecial b2 (18,2) Black in
  let b2 = Myboard.insertspecial b2 (16,18) Black in
  let b2 = Myboard.insertspecial b2 (17,18) Black in
  let b2 = Myboard.insertspecial b2 (0,18) Black in
  let b2 = Myboard.insertspecial b2 (0,17) Black in
  let b2 = Myboard.insertspecial b2 (0,16) Black in
  let b2 = Myboard.insertspecial b2 (1,18) Black in
  let b2 = Myboard.insertspecial b2 (2,18) Black in
  let b2 = Myboard.insertspecial b2 (5,17) Black in
  let b2 = Myboard.insertspecial b2 (6,16) Black in
  let b2 = Myboard.insertspecial b2 (8,13) Black in
  let b2 = Myboard.insertspecial b2 (8,12) Black in
  let b2 = Myboard.insertspecial b2 (9,13) White in
  let b2 = Myboard.insertspecial b2 (8,16) White in
  let b2 = Myboard.insertspecial b2 (8,9) White in
  assert(BThreats.evaluate_board b2 <> None) ;
  successful := !successful + 1 ;

  (* board of figure 4 of paper used in paper to demonstrate merge function*)
  let b3 = Myboard.empty in
  let b3 = Myboard.insertspecial b3 (6,6) Black in
  let b3 = Myboard.insertspecial b3 (7,7) Black in
  let b3 = Myboard.insertspecial b3 (8,8) Black in
  let b3 = Myboard.insertspecial b3 (8,7) Black in
  let b3 = Myboard.insertspecial b3 (8,6) Black in
  let b3 = Myboard.insertspecial b3 (7,11) Black in
  let b3 = Myboard.insertspecial b3 (5,5) White in
  let b3 = Myboard.insertspecial b3 (7,6) White in
  let b3 = Myboard.insertspecial b3 (8,5) White in
  let b3 = Myboard.insertspecial b3 (6,7) White in
  let b3 = Myboard.insertspecial b3 (7,8) White in
  let b3 = Myboard.insertspecial b3 (12,6) White in
  assert(BThreats.evaluate_board b3 <> None) ;
  successful := !successful + 1 

let _ = test_evaluate_board ();
  print_string "\nevaluateBoard successfully passed: ";
  print_int !successful;
  print_string "/3 tests.\n\n";

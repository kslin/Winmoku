(* This file contains files with known threat sequences for testing *)

open Board
open Boardstuffs

let threatseq2 (b:Myboard.board) = 
	let b1 = Myboard.insertspecial b (10,10) Black in
	let b1 = Myboard.insertspecial b1 (9,10) Black in
	let b1 = Myboard.insertspecial b1 (11,10) Black in
	let b1 = Myboard.insertspecial b1 (10,9) Black in
	let b1 = Myboard.insertspecial b1 (10,8) Black in
	let b1 = Myboard.insertspecial b1 (9,8) Black in
	let b1 = Myboard.insertspecial b1 (11,8) Black in
	let b1 = Myboard.insertspecial b1 (9,7) Black in
	let b1 = Myboard.insertspecial b1 (9,6) Black in
	let b1 = Myboard.insertspecial b1 (8,6) Black in
	let b1 = Myboard.insertspecial b1 (10,6) Black in
	let b1 = Myboard.insertspecial b1 (8,11) Black in
	let b1 = Myboard.insertspecial b1 (10,11) Black in
	let b1 = Myboard.insertspecial b1 (8,10) White in
	let b1 = Myboard.insertspecial b1 (8,9) White in
	let b1 = Myboard.insertspecial b1 (9,9) White in
	let b1 = Myboard.insertspecial b1 (8,8) White in
	let b1 = Myboard.insertspecial b1 (8,7) White in
	let b1 = Myboard.insertspecial b1 (7,6) White in
	let b1 = Myboard.insertspecial b1 (9,5) White in
	let b1 = Myboard.insertspecial b1 (10,7) White in
	let b1 = Myboard.insertspecial b1 (12,7) White in
	let b1 = Myboard.insertspecial b1 (12,8) White in
	let b1 = Myboard.insertspecial b1 (11,9) White in
	let b1 = Myboard.insertspecial b1 (7,12) White in
	let b1 = Myboard.insertspecial b1 (10,12) White in
	b1

let threatseq3a (b:Myboard.board) =
	let b1 = Myboard.insertspecial b (0,0) Black in
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
	b1

let threatseq3b (b:Myboard.board) =
	let b1 = Myboard.insertspecial b (0,0) Black in
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
	let b1 = Myboard.insertspecial b1 (5,17) Black in
	let b1 = Myboard.insertspecial b1 (6,16) Black in
	let b1 = Myboard.insertspecial b1 (8,13) Black in
	let b1 = Myboard.insertspecial b1 (8,12) Black in
	let b1 = Myboard.insertspecial b1 (9,13) White in
	let b1 = Myboard.insertspecial b1 (8,16) White in
	let b1 = Myboard.insertspecial b1 (8,9) White in
	b1

let threatseq4 (b:Myboard.board) =
	let b1 = Myboard.insertspecial b (6,6) Black in
	let b1 = Myboard.insertspecial b1 (7,7) Black in
	let b1 = Myboard.insertspecial b1 (8,8) Black in
	let b1 = Myboard.insertspecial b1 (8,7) Black in
	let b1 = Myboard.insertspecial b1 (8,6) Black in
	let b1 = Myboard.insertspecial b1 (7,11) Black in
	let b1 = Myboard.insertspecial b1 (5,5) White in
	let b1 = Myboard.insertspecial b1 (7,6) White in
	let b1 = Myboard.insertspecial b1 (8,5) White in
	let b1 = Myboard.insertspecial b1 (6,7) White in
	let b1 = Myboard.insertspecial b1 (7,8) White in
	let b1 = Myboard.insertspecial b1 (12,6) White in
	b1

let threatseq5 (b:Myboard.board) = 
	let b1 = Myboard.insertspecial b (9,9) Black in
	let b1 = Myboard.insertspecial b1 (9,10) Black in
	let b1 = Myboard.insertspecial b1 (10,9) Black in
	let b1 = Myboard.insertspecial b1 (11,9) Black in
	let b1 = Myboard.insertspecial b1 (8,9) White in
	let b1 = Myboard.insertspecial b1 (7,9) White in
	let b1 = Myboard.insertspecial b1 (9,11) White in
	let b1 = Myboard.insertspecial b1 (10,10) White in
	b1 

let threatseq : Myboard.board list =
	let bor = Myboard.empty in
<<<<<<< HEAD
	let bor1 = threatseq2a bor in
	let bor2 = threatseq2b bor in
	let bor3 = threatseq3b bor in
	let bor4 = threatseq4 bor in
	[bor1;bor2;bor3;bor4]
=======
	[threatseq2 bor; threatseq3b bor; threatseq4 bor; threatseq5 bor]
>>>>>>> 1875203c3e65d2723d202d16c748d1bfe85bc09d

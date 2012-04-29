open Board
open Boardstuffs

let threatseq1 (b:Myboard.board) = 
	let b1 = Myboard.insertspecial b (7,11) Black in
	let b1 = Myboard.insertspecial b1 (5,11) Black in
	let b1 = Myboard.insertspecial b1 (6,12) Black in
	let b1 = Myboard.insertspecial b1 (7,13) Black in
	let b1 = Myboard.insertspecial b1 (9,13) Black in
	let b1 = Myboard.insertspecial b1 (8,14) Black in
	let b1 = Myboard.insertspecial b1 (10,14) Black in
	let b1 = Myboard.insertspecial b1 (9,14) White in
	let b1 = Myboard.insertspecial b1 (8,13) White in
	let b1 = Myboard.insertspecial b1 (7,12) White in
	let b1 = Myboard.insertspecial b1 (6,10) White in
	let b1 = Myboard.insertspecial b1 (9,15) White in
	let b1 = Myboard.insertspecial b1 (4,10) White in
	b1

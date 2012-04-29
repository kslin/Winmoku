open Board
open Boardstuffs

let threatseq1 (b:Myboard.board) = 
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
	b1
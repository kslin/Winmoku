open Board
open Boardstuffs
open Threats
(* Module for minimax algorithm, modified to suit gomoku. *)

module type MINIMAX = 
sig
  (* game board *)
  type board
  
  (* one threat *)
  type threat
  
  (* a collection of threats *)
  type threats
  
  (* game tree *)
  type tree

  (* depth for minimax *)
  val depth : int

  (* heuristic to rate board *)
  val heuristic : board -> float

  (* generate a tree *)
  val gen_tree : int -> index option -> board -> tree

  (* run minimax algorithm on tree *)
  val minimax : tree -> tree

  (* pick best path on tree *)
  val next_move : tree -> index option

end

module MGenerator(B: BOARD):MINIMAX with type board = B.board 
                                    and type threat = Boardstuffs.threat 
                                    and type threats = threat list =
  struct
    type board = B.board
    type threat = Boardstuffs.threat
    type threats = threat list
    type tree = 
      | Node of board * index option * tree list * float option
      | Leaf of board * index option * float option

  let depth = 2

  let branchingfactor = 10

  let heuristic board = 

  let rec gen_tree depth index board = 
    if depth > 0 then
      let range = Boardstuffs.range 0 (Boardstuffs.world_size - 1) in
      let coords = Boardstuffs.cross range range in
      let sorter index = 
        if (B.get board index != Unocc) then -1.0
        else heuristic (B.insert board index)
      in
      let candidates = List.filter (fun x -> (sorter x) > -1.0) coords in
      let sorted_cand = List.sort (fun x y -> compare (sorter x) (sorter y))
                                  candidates
      in
      let rec take lst x =
        if x > 0 then match lst with
                      | [] -> []
                      | hd::tl -> hd::(take tl (x-1))
        else [] 
      in
      let moves = take sorted_cand branchingfactor in
      let boards = List.map (fun x -> ((B.insert board x), Some x)) moves in
      let trees = List.map (fun (x, y) -> gen_tree (depth - 1) y x) boards in
        if trees = [] then Leaf(board, index, None)
        else Node(board, index, trees, None)
    else
      Leaf(board, index, None)

  let rec minimax tree =
    let h = if ((depth mod 2) = 1) then (fun x -> -.(heuristic x)) else heuristic
    in
    match tree with
    | Leaf(board, index, _) -> Leaf(board, index, Some (h board)) 
    | Node(board, index, tlist, value) -> 
      (let ntlist = List.map minimax tlist in
       let value = List.fold_left max (-1.0) 
                    (List.map (fun x -> match x with
                                        | Leaf(_, _, Some v) -> -.v
                                        | Node(_, _, _, Some v) -> -.v)
                               ntlist) 
       in
         Node(board, index, ntlist, Some value))
  
  let rec print_treelist tlist = 
    match tlist with
    | hd::tl -> (match hd with
                 | Leaf(_, _, Some v) -> (print_float v; print_treelist tl)
                 | Node(_, _, _, Some v) -> (print_float v; print_treelist tl))
    | [] -> print_string "noooo"

  let next_move tree =
    match tree with
    | Leaf(_, _, _) -> None
    | Node(_, _, tlist, Some value) ->
      (let lst = (List.filter (fun x -> match x with
                                        | Leaf(_,_,Some v) -> (v = (-.value))
                                        | Node(_,_,_,Some v) -> (v = (-.value)))
                              tlist) 
       in
         match lst with
         | [] -> (print_treelist tlist; print_string "|"; print_float value; None)
         | hd::tl -> (match hd with
                      | Leaf(_,index,_) -> index  
                      | Node(_,index, _, _) -> index))

end

module GMinimax = MGenerator(Myboard)

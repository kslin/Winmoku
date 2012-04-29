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

  (* heuristic to rate board *)
  val heuristic : board -> float

  (* generate a tree *)
  val gen_tree : board -> tree

  (* run minimax algorithm on tree *)
  val minimax : tree -> (board -> float) -> tree

  (* pick best path on tree *)
  val next_move : tree -> index option

end

module MGenerator(B: BOARD) with type board = B.board 
                            and type threat = Boardstuffs.threat 
                            and type threats = threat list =
  struct
    type board = B.board
    type threat = Boardstuffs.threat
    type threats = threat list
    type tree = 
      | Node of board * index * tree list * float option
      | Leaf of board * index * float option

  let depth = 5

  let branchingfactor = 10

  let heuristic board = 1

  let rec gen_tree depth index board = 
    if depth > 0 then
      let range = Boardstuffs.range 0 (Boardstuffs.world_size - 1) in
      let coords = Boardstuffs.cross range range in
      let sorter index = 
        if (B.get b index != Unocc) then -1
        else heuristic (B.insert b index)
      in
      let candidates = List.filter (fun x -> (sorter x) > -1) coords in
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
      let boards = List.map (fun x -> ((B.insert b x), Some x)) moves in
      let trees = List.map (fun (x, y) -> gen_tree (depth - 1) y x) boards in
        if trees == [] then Leaf(board, index, None)
        else Node(board, trees, index, None)
    else
      Leaf(board, index, None)

  let rec minimax tree =
    let h = if (depth % 2 == 1) then (fun x -> - (heuristic x)) else heuristic
    match tree with
    | Leaf(board, index, _) -> Leaf(board, index, Some (h board)) 
    | Node(board, tlist, index, value) -> 
      (let value = List.fold_left max (-1) 
                    (List.map (fun x -> match (minimax x) with
                                        | Leaf(_, _, Some v) -> -v
                                        | Node(_, _, _, Some v) -> -v)
                              tlist) 
       in
         Node(board, tlist, index, Some value))
  
  let next_move tree =
    match tree with
    | Leaf(_, _, _) -> None
    | Node(_, tlist, _, Some value) ->
      (let lst = (List.filter (fun x -> match x with
                                        | Leaf(_, _,Some v) -> (v == -value)
                                        | Node(_, _, _,Some v) -> (v == -value))
                              tlist) 
       in
         match lst with
         | [] -> (print_string "broken"; None)
         | hd::tl -> (match hd with
                      | Leaf(_,index,_) -> Some index  
                      | Node(_,_,index,_) -> Some index))

end

module GomokuMiniMax = PGenerator(BThreats)

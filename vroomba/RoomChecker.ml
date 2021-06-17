
(*
This file is part of teaching material of Yale-NUS College module
"YSC2229: Introductory Data Structures and Algorithms"

Copyright (c) 2020 Ilya Sergey

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

open Util
open Rooms
open Graphs
open BetterHashTable
open RoomGenerator
open RoomUtil 

module HygieneTable = 
  ResizableListBasedHashTable(struct type t = (int * int) end)

(*********************************************)
(*         Movements of Vroomba              *)
(*********************************************)

type move = 
  | Up 
  | Left 
  | Down 
  | Right

(* Print the move *)
let pp_move = function
  | Up -> "W"
  | Left -> "A"
  | Down -> "S"
  | Right -> "D"

(* This is a data type, representing the state of the room at a
   certain point of cleaning. It should include the room and keep
   track of Vroomba's current position, parts that are already cleaned
   and that are remaining to be cleaned. Use this data type internally
   in the function `check_solution` *)

type hygiene = 
  | Dirty
  | Clean

type state = {
 current : (int * int) ref ;
 table : ((int * int) * hygiene) HygieneTable.hash_table;
 dirty_tiles : int ref
}

(* A quick way to check if a coor is a tile, given the state; ~O(1) *)
let is_a_tile state coor = HygieneTable.get state.table coor != None

let initiate_state room =
  let num = get_tiles_num room in
  let all_tiles = get_all_tiles room in
  let ht = HygieneTable.mk_new_table num in
  List.iter (fun coor -> HygieneTable.insert ht coor Dirty) all_tiles;
  let starting_point = (0,0) in
  { 
    current  = ref starting_point;
    table =  ht;
    dirty_tiles = ref num
  }

(*********************************************)
(*            Checking solution              *)
(*********************************************)

(*  Get a trace of Vroomba from a string  *)
(*  A string can be ill-formed! *)
let string_to_solution (s: string) : move list option = 
  let len = String.length s in
  let res = ref [] in
  try (for i = 0 to len - 1 do
         let move = 
           match s.[i] with
           | 'W' -> Up
           | 'A' -> Left
           | 'S' -> Down
           | 'D' -> Right
           | _ -> error "Unrecognizable move!" in
         res := move :: !res
       done;
       Some (List.rev !res))
  with error ->
    None

let write_solution_to_file (moves : move list) (path : string) : unit = 
  let buffer = Buffer.create 1 in
  List.iter (fun e -> Buffer.add_string buffer (pp_move e)) moves;
  ReadingFiles.write_string_to_file path (Buffer.contents buffer)

let moves_to_string ls =
  let buffer = Buffer.create 1 in
  List.iter (fun m -> Buffer.add_string buffer (pp_move m)) ls;
  Buffer.contents buffer

let move_in_dir coor dir =
  let (x, y ) = coor in
  match dir with 
  | Up -> (x , y + 1)
  | Down -> (x , y - 1)
  | Left -> (x - 1 , y)
  | Right -> (x + 1 , y)

(* RUI: 
 * When cleaning a tile:
 * Mark the hygeine status as Clean & change dirty_tiles if the tile is Dirty
 * 
 * 1. At current location, check the coordinates:
 * - If does not exist in room -> fail game
 * - If exists, change hygeine state to Clean in hashtable & dirty_tiles - 1
 * 
 * 2. Check the 8 neighbouring coordinates:
 * - If any coordinate does not exist in room -> ignore
 * - For those that are in room:
 *   - If not reachable: ignore
 *   - If reacheable: clean
 *
 * 3. Move the current point in the state *)

let clean_a_tile state coor = 
  let ht = state.table in 
  let hg = get_exn (HygieneTable.get ht coor) in
  if hg = Dirty
  then
  state.dirty_tiles := !(state.dirty_tiles) - 1;
  HygieneTable.insert ht coor Clean

(*  Check that the sequence of moves is valid  *)
let check_solution (r: room) (moves: move list) : bool = 
  let move_list = ref moves in
  let remaining = ref ((List.length moves) + 1) in
  let state = initiate_state r in
  if !remaining * 9 < !(state.dirty_tiles)
  then false
  else begin
    let success = ref true in
    while (!success && !remaining > 0) do
      (* Check current point *)
      let curr = !(state.current) in
      if not (is_a_tile state curr)
      then success := false
      else clean_a_tile state curr;

      (* Check the eight neighbors *)
      let neighbors = get_eight_neighbors curr in
      List.iter (fun coor -> 
          if is_a_tile state coor
          then (if reachable r curr coor
                then clean_a_tile state coor)
        ) neighbors;

      (* move Vroomba and update move_list  *)
      remaining := !remaining - 1;
      if !remaining > 0
      then (let dir = List.hd !move_list in
            state.current := move_in_dir curr dir;
            move_list := List.tl !move_list)
    done;
    if !(state.dirty_tiles) > 0 
    then false
    else !success
  end


(*  Top-level validator  *)
let validate r s = 
  match string_to_solution s with
  | None -> false
  | Some moves -> check_solution r moves 

(* Function for runner *)
let check_runner input_file solutions_file =
  let polygon_ls = file_to_polygons input_file in
  let solutions_ls = BinaryEncodings.read_file_to_strings solutions_file in
  (* catch error: not enough solutions for # of rooms, or vice versa *)
  let p_len = List.length polygon_ls in
  let s_len = List.length solutions_ls in
  if p_len < s_len
  then Printf.printf
      "More solutions than rooms! Are you sure you have the right files?"
  else begin
    let solutions_ls' =
      if s_len < p_len
      then (let added = List.init (p_len - s_len) (fun n -> "") in
            List.append solutions_ls added)
      else solutions_ls in

    (* validate each solution *)
    let num = ref 1 in
    List.iter2 (fun p s ->
        let r = polygon_to_room p in
        if validate r s
        then Printf.printf "%d: %d \n" !num (String.length s)
        else Printf.printf "%d: Fail \n" !num;
        num := !num + 1) polygon_ls solutions_ls'
  end


(* Tests *)
  
let%test "test_initial_state" =
  let input  = BinaryEncodings.find_file "../../../resources/basic.txt" in
  let polygon_list = file_to_polygons input in
  let p = List.hd polygon_list in
  let room = polygon_to_room p in 
  let state = initiate_state room in
  !(state.current) = (0,0) &&
  !(state.dirty_tiles) = 20

let%test _ = 
  let s = "(0, 0); (6, 0); (6, 1); (8, 1); (8, 2); (6, 2); (6, 3); (0, 3)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  validate room "WDDDDDD" 

let%test "string_to_solution & moves_to_string 1" =
  let input  = BinaryEncodings.find_file "../../../resources/basic.sol" in
  let s = BinaryEncodings.read_file_to_single_string input in
  let moves = get_exn @@ string_to_solution s in
  let s' = moves_to_string moves in
  s = s'

let%test "string_to_solution & moves_to_string 2" =
  let input  = BinaryEncodings.find_file "../../../resources/rooms.sol" in
  let ls = BinaryEncodings.read_file_to_strings input in
  List.for_all (fun s ->
      let moves = get_exn @@ string_to_solution s in
      let s' = moves_to_string moves in
      s = s') ls

let%test "test_checker_simple 1" = 
  let s = "(0, 0); (1, 0); (1, 1); (0, 1)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  validate room ""
    
let%test "test_checker_simple 2" = 
  let s = "(0, 0); (2, 0); (2, 2); (0, 2)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  validate room "" 

let%test "test_checker_simple 3" = 
  let s = "(0, 0); (1, 0); (1, 1); (2, 1); (2, 2); (0, 2)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  validate room "W" 

let%test "test_checker_simple 3 neg" = 
  let s = "(0, 0); (1, 0); (1, 1); (2, 1); (2, 2); (0, 2)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  not (validate room "") 

let%test "test_checker_simple 4" = 
  let s = "(0, 0); (0, 3); (1, 3); (1, 0)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  validate room "W" 

let%test "test_checker_simple 4 neg" = 
  let s = "(0, 0); (0, 3); (1, 3); (1, 0)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  not (validate room "") 

let%test "test_checker_basic_negative" = 
  let s = "(0, 0); (6, 0); (6, 1); (8, 1); (8, 2); (6, 2); (6, 3); (0, 3)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  not (validate room "WWWWDDDDD") &&
  not (validate room "WWDDAD")

let%test "test_checker_rooms_negative" = 
  let input  = BinaryEncodings.find_file "../../../resources/rooms.txt" in
  let polygon_list = file_to_polygons input in
  List.for_all (fun p -> 
                    let room = polygon_to_room p in 
                    not (validate room "W") ) 
  polygon_list

let%test "test_checker_rooms_negative" = 
  let input  = BinaryEncodings.find_file "../../../resources/rooms.txt" in
  let polygon_list = file_to_polygons input in
  List.for_all (fun p -> 
                    let room = polygon_to_room p in 
                    not (validate room "W") ) 
  polygon_list

let%test "test_checker_random_negative" = 
  let room = generate_random_room 100 in
  not (validate room "W")

let%test "test_checker_invalid room" = 
  let s = "(0, 0); (1, 0); (1, 1); (2, 1); (2, 2); (0, 2)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  not (validate room "k")
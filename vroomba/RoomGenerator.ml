(*
This file is part of teaching material of Yale-NUS College module
"YSC2229: Introductory Data Structures and Algorithms"

Copyright (c) 2021 Ilya Sergey

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
open Polygons
open ArrayUtil
open RoomUtil
(*********************************************)
(*       Automated generation of rooms       *)
(*********************************************)

(*  size is the maximal span of the room along both X and Y dimensions *)
(*  Example generate_random_room 4 should return a room that fits into a
    4x4 square. *)

(* RUI: The idea is to generate a valid polygon, 
 * and use polygon_to_room to convert it to room. We can break the room into
 * 4 quadrants. In each quadrant we plot the room 's boundaries by moving a 
 * point in three directions.
   
 * If the move's direction is diff from the previous one, record the 
 * turning point
 * Finally, we shift the room so that (0,0) falls on a random point in 
 * the room. Restrictions on the point's movement are set so as to 
 * prevent edges from crossing.

FIRST QUADRANT (Top Left):
- Start at the left-most point of the empty space, on x-axis
- First move must be Up
- Move the point Up, Down, or Right within the boundary
- Can't touch the negative x-axis
- Until reaching the positive y-axis

SECOND QUADRANT (Top Right):
- First move must be Right
- Move the point Down, Left, or Right within the boundary
- Can't touch the positive y-axis
- Until reaching the positive x-axis

THIRD QUADRANT (Bottom Right):
- First move must be Down
- Move the point Up, Down, or Left within the boundary
- Can't touch the positive x-axis
- Until reaching the negative y-axis

Fourth QUADRANT (Bottom Left):
- First move must be Left
- Move the point Up, Left, or Right within the boundary
- Can't touch the negative y-axis
- Until reaching the negative x-axis
- If last point = initial point -> do not add last point to corner list

 * Some limitations on moving directions & steps in each quadrant
 * - The maximum number of steps allowed to take in a direction is 
 *   limited by the boundary 
 * - If the previous move is Up, the next can't be Down, etc. 
 *   Basically, can't "cancel" the previous move *)

type quadrant =
  | First
  | Second
  | Third
  | Fourth

let four_quadrants = [|First; Second; Third; Fourth|]

(* Given a quadrant and the room size (side length),
 * return boundaries of movements *)
let get_boundaries quad size = 
  let half = size / 2 in
  let half' = size - half in
  match quad with
  | First -> (half, 1, -half, 0)
  | Second -> (half, 0, 1, half')
  | Third -> (-1, -half', 0, half')
  | Fourth -> (0, -half', -half, -1)

(* Given a coordinate and the quadrant, return if should stop *)
let should_stop coor quad = 
  let (x, y) = coor in 
  match quad with 
  | First -> x = 0
  | Second -> y = 0
  | Third -> x = 0
  | Fourth -> y = 0

(* the possible directions to take in each quadrant *)
let get_all_directions quad = 
  match quad with
  | First -> [|Up; Down; Right|]
  | Second -> [|Down;Left; Right|]
  | Third -> [|Up; Down; Left|]
  | Fourth -> [|Up; Left; Right|]

(* get the allowed first direction in the quadrant *)
let get_first_direction quad = 
  match quad with
  | First -> Up
  | Second -> Right
  | Third -> Down
  | Fourth -> Left

(* Given a coordinate and the boundaries, return allowed
 * moves in all four directions *)
let get_allowed_moves coor boundaries = 
    let (x, y ) = coor in
    let (up_bound, low_bound, left_bound, right_bound) = boundaries in
    let up_move = abs (up_bound - y)
    and down_move = abs (low_bound - y)
    and left_move = abs (left_bound - x)
    and right_move = abs (right_bound - x)
    in 
    [|up_move; down_move; left_move ; right_move|] 

(* given an array of maximum movements in all directions and a direction, 
 * return the maximum no. of steps can take in that direction*)
let get_max_steps_in_direction allowed_moves dir = 
  match dir with
  | Up -> allowed_moves.(0)
  | Down -> allowed_moves.(1)
  | Left -> allowed_moves.(2)
  | Right -> allowed_moves.(3)
  | _ -> error "Invalid direction."

(* given a coordinate, the direction, and the number of steps,
 * return the final position of the coordinates *)
let take_steps_in_dir coor steps dir =
  let (x, y ) = coor in
  match dir with 
  | Up -> (x , y + steps)
  | Down -> (x , y - steps)
  | Left -> (x - steps , y)
  | Right -> (x + steps , y)
  | _ -> error "Invalid direction."  

(* relocate (0,0) along x or y axis. shift the whole polygon *)
let relocate_starting_point polygon = 
  let p = polygon_to_int_pairs polygon in
  let room = polygon_to_room polygon in
  let tiles = get_all_tiles room in
  let len = List.length tiles in
  let (x, y) = List.nth tiles (Random.int len) in
  let polygon' = List.map (fun (a, b) -> (a - x, b - y)) p in
  polygon_of_int_pairs polygon' 

let generate_random_room (size : int) : room = 

  (* move in quadrant, return final position, last 
   * direction taken and the corner list*)

  let move_in_quadrant init_coor prev_dir quad corner_list = 
    let boundaries = get_boundaries quad size in 

    (* Since making the first move & following moves share similar procedures,
     * encode them in this generic move function  *)
    let generic_move allowed_directions random coor prev_dir corner_list =

      let allowed_moves = get_allowed_moves coor boundaries in 
      let dir = let i = Random.int(random) in allowed_directions.(i) in
 
      let max_moves = get_max_steps_in_direction allowed_moves dir in      
      let steps = if max_moves = 0 then 0 else (Random.int (max_moves) +1) in 
      let new_coor = take_steps_in_dir coor steps dir in

      (* if the current direction is different from prev_dir, the point has 
       * taken a turn. We record the previous point in the corner list *)    
      let new_corner_list = 
          if (dir != prev_dir) && steps > 0
          then coor :: corner_list
          else corner_list
      in let dir' = if steps = 0 then prev_dir else dir
      in (new_coor, dir', new_corner_list)

    in
      let make_first_move () =  
        let allowed_dir = [|get_first_direction quad|] in
        generic_move allowed_dir 1 init_coor prev_dir corner_list
    in
      let make_next_move coor prev_dir corner_list =
        (* if prev_dir is Down, dir can't be Up and so on *)
        let allowed_dir_ls = array_to_list (get_all_directions quad) in
        let allowed_dir = 
            list_to_array 
            (List.filter (fun d -> 
                          match prev_dir with
                          | Up -> d != Down
                          | Down -> d!= Up
                          | Left -> d!= Right
                          | Right -> d!= Left
                          | _ -> true) 
            allowed_dir_ls) in
        generic_move allowed_dir 2 coor prev_dir corner_list
    in
      let rec make_following_moves coor prev_dir corner_list stop =
        if stop 
        then (coor, prev_dir, corner_list)
        else 
        let (new_coor, dir, new_corner_list) = make_next_move coor prev_dir corner_list in 
          if should_stop new_coor quad 
          then (new_coor, dir, new_corner_list)
          else make_following_moves new_coor dir new_corner_list false
    in 

      let (new_coor, dir, new_corner_list) = make_first_move () in
      if should_stop new_coor quad 
      then (new_coor, dir, new_corner_list)
      else 
      make_following_moves new_coor dir new_corner_list false
  in

  let rec move_through_four_quadrants init_coor prev_dir corner_list num =
    if num = 4
    then (init_coor, prev_dir, corner_list)
    else
    let quad = four_quadrants.(num) in
    let (new_coor, dir, new_corner_list) = move_in_quadrant init_coor prev_dir quad corner_list
    in 
    move_through_four_quadrants new_coor dir new_corner_list (num + 1)
  
  in 


  let initial_point = (- (size/2) , 0) in
  let final_coor, last_dir, corner_list =
      move_through_four_quadrants initial_point Up [initial_point] 0
  in

    let final_corner_list =
    if final_coor = initial_point
    then corner_list
    else final_coor :: corner_list
  in 
  (* Convert corner list to polygon & shift (0, 0) to a 
   * random but valid starting point *)
    let polygon = polygon_of_int_pairs final_corner_list in
    let polygon' = relocate_starting_point polygon in
    let room = polygon_to_room polygon' in

    room


(* Define what it means to the room to be valid (e.g., no lacunas,
   obstacles, there is a place for initial Vroomba position, etc). *)

(* RUI: A few checks:

 * 1. No lacunas: 
 * In our implementation, drawing a lacuna will end up 
 * with edges extending out from existing edges that not 
 * adjacent to it. So here we just need to check if edge intersect
 * with another edge that is not adjacent to it

 * 2. No obstacles : O(n^2)
 * Same as above

 * 3. Initial place for Vroomba: O(1)
 * (0,0) must be cleanable (aka. is a tile)

 * 4. No diagonal edges: 
 * Consecutive edges only run horiontally or vertically
 * only change in x or y coordinate
 * this will throw error in polygon_to_room

 * 5. No collinear edges:
 * Check for non-collinearity while checking #1

 * 6. No "8" shaped rooms:
 * Same checks as #1
   
*)


(* find the direction from point 1 to point 2*)

let valid_room (r: room) : bool = 
  let polygon = room_to_polygon r in 
  let len = List.length !(r.edges) in

  (* A room cannot have less than 4 edges *)
  if len <= 3 then false 
  else begin
    
    (*get edge list in (Point * Point) pairs *)
    let edge_list = Polygons.edges polygon in
    let edge_arr = list_to_array edge_list in
    
    let no_intersect_or_collinear = 
      let res = ref true in 
      let i = ref 0 in 
      while !res && !i < len - 1 do
        let s1 = edge_arr.(!i) in 
        (* s1 vs all segments behind it excluding the next neighbor *)
        for j = !i+2 to len - 1 do
          (* avoid index out of bounds *)
          if j <= len - 1
          then begin
            (* skip first and last segment comparison *)
            if !i = 0 && j = len -1 then () 
            else
              (let s2 = edge_arr.(j) in
               if (segments_intersect s1 s2) ||
                  (intersect_as_collinear s1 s2)
               then res := false 
               else ())  
          end
        done;
        
        (* s1 vs all segments before it excluding the previous neighbor *)
        for j = !i-2 downto 0 do
          (* avoid index out of bounds *)
          if j >= 0 then
            begin
              (* skip first and last segment comparison *)
              if !i = len -1 && j = 0 then () 
              else
                (let s2 = edge_arr.(j) in
                 if (segments_intersect s1 s2) ||
                    (intersect_as_collinear s1 s2)
                 then res := false)
            end
        done;
        i := !i + 1
      done;
      !res
    in
    
    let space_for_vroomba = cleanable r (0,0) in 
    no_intersect_or_collinear && space_for_vroomba
  end
  
  
(*********************************************)
(*                     Tests                 *)
(*********************************************)


let%test "Generated room is valid" = 
  for _ = 0 to 20 do
    let size = 2 + Random.int 49 in
    let r = generate_random_room size in
    assert (valid_room r)
  done;
  true

let%test "test_valid_room_simple" = 
  let input  = BinaryEncodings.find_file "../../../resources/basic.txt" in
  let polygon_list = file_to_polygons input in
  List.for_all (fun p -> 
                    let room = polygon_to_room p in  
                    valid_room room) 
  polygon_list

let%test "test_valid_room_simple_negative" = 
  let input  = BinaryEncodings.find_file "../../../resources/invalid.txt" in
  let polygon_list = file_to_polygons input in
  List.for_all (fun p -> 
                    (* print_endline "\n\nCHECKING POLYGON\n"; *)
                    try (let room = polygon_to_room p in  
                    not (valid_room room))
                    with Failure _ -> true) 
  polygon_list

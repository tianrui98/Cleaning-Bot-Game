(* This file is part of the final project of Yale-NUS College module
 * "YSC2229: Introductory Data Structures and Algorithms" *)

open Util
open Rooms
open Polygons

(* Helper functions *)

(* ========Point - Coordinate - Polygon - pos=======*)

let point_to_coor (Point (x, y)) = 
  (int_of_float x,int_of_float y)

let coor_to_point (x, y) =
  Point (float_of_int x, float_of_int y)

(* convert coordinates to room.map indices *)
let coor_to_map_index room (x, y) = 
  let (minx, miny) = !(room.shift) in
  (x - minx, y - miny)

let map_index_to_coor room (x, y) = 
  let (minx, miny) = !(room.shift) in
  (x + minx, y + miny)

let get_pos room (x, y) =
  let (x', y') = coor_to_map_index room (x, y) in
  room.map.(x').(y')

(* convert a list of points to a list of (int * int) *)
let polygon_to_int_pairs polygon = 
  let int_pairs = ref [] in 
  List.iter (fun p -> 
      let (x, y) = point_to_coor p in 
      int_pairs := (x, y) :: !int_pairs
    ) polygon;
  List.rev !int_pairs

(* ========Direction======= *)
    
type direction = 
  | Up
  | Down
  | Left
  | Right
  | Diagonal
  | Stop

(* ========Room coordinates - tiles - reachability======= *)

(* get all coordinates of the room & outside space *)
let get_all_points room =
  let map = room.map in 
  let len = Array.length map in
  let all_points = ref [] in 
  for x = 0 to len - 1 do 
    for y = 0 to len - 1 do 
    let coor = map_index_to_coor room (x,y) in
    all_points := coor :: !all_points
    done
  done;
  !all_points

(* retrieve the coordinates of the points in the same tile *)
let get_three_corners (x, y) = 
  let n1 = (x, y + 1)
  and n2 = (x + 1, y)
  and n3 = (x + 1, y +1) in
  [n1; n2; n3]

(* retrieve the coordinates of four direct neighbors *)
let get_four_neighbors (x, y) =
  let n1 = (x, y + 1)
  and n2 = (x + 1, y)
  and n3 = (x , y - 1)
  and n4 = (x - 1, y) in 
  [n1; n2; n3; n4]

(* retrieve the coordinates of the neighboring tiles *)
let get_eight_neighbors (x, y) = 
  let n1 = (x, y + 1)
  and n2 = (x + 1, y)
  and n3 = (x + 1, y + 1)
  and n4 = (x + 1, y - 1)
  and n5 = (x, y - 1)
  and n6 = (x - 1, y - 1)
  and n7 = (x - 1, y)
  and n8 = (x - 1, y + 1)
  in
  [n1; n2; n3; n4; n5; n6; n7; n8]


(* **************** ZITING'S ADDITION **************** *)
(* **************** From here onwards **************** *)

(* If a coordinate exists in the room & non-room space aka the board/map *)

(* A less expensive version *)
(* function takes map indices *)
let exist_in_room_no_shift room relative_coor = 
  let len = Array.length room.map in 
  let (x,y) = relative_coor in 
  0 <= x && x < len && 0 <= y && y < len

(* function takes coordiantes *)
let exist_in_room room coor = 
  let relative_coor = coor_to_map_index room coor in
  exist_in_room_no_shift room relative_coor 

let get_edges_no_shift room =
  let (shift_x, shift_y) = !(room.shift) in
  !(room.edges) |> List.map (fun (x,y) -> (- shift_x + x, - shift_y + y))

let get_pos_no_shift room (x,y) = 
  room.map.(x).(y)


(* RUI: is a tile at coor cleanable? aka. is a room tile?
 *
 * A tile is cleanable if:
 * 1. The pos is Inner; or
 * 2. The pos is Edge && (Other 3 pos in the same square are not Outer)
 * 3. If all four corners are Edge:

 * ZITING: If a tile's 4 corners are on the edges, check whether the centre
   of the tile is within the polygon  *)

let cleanable room coor = 
  if not (exist_in_room room coor) 
  then false
  else begin
    let p = get_pos room coor in 
    match p with 
    | Outer -> false
    | Inner -> true
    | Edge -> 
      (* If the current coor is Edge, check its three coreners *)
      (let corners = get_three_corners coor in
       if (List.for_all (fun n -> exist_in_room room n) corners)
       then begin
         (* if all three corners are edges, 
          * check the center of the tile *)
         if List.for_all (fun n-> (get_pos room n) = Edge) corners
         then (let center = get_center (coor_to_point coor) in
               let polygon = room_to_polygon room in
               point_within_polygon_2 polygon center)
              
         (* if not all three corners are edges, 
          * check if they are all not Outer *)
         else List.for_all (fun n -> (get_pos room n != Outer)) corners
       end
       (* if any corner does not exist, 
        * the current is not a tile  *)
       else false)
  end 

(* Ziting: Same as cleanable except for that cleanable takes the 
  * absolute coordinates of a square's left bottom corner, while 
   *  no_shift takes relative coordinates aka the array index. *)
let cleanable_no_shift room relative_coor =
  let coor = coor_to_map_index room relative_coor in
  cleanable room coor 

(* Get the coordinates of all tiles *)
let get_all_tiles room =
  let tiles = ref [] in
  let map = room.map in
  let len = Array.length map in
  for x = 0 to len - 1 do 
    for y = 0 to len - 1 do 
      let (a, b) = map_index_to_coor room (x,y) in
      if cleanable room (a,b)
      then tiles := (a, b) :: !tiles
    done;
  done;
  !tiles 

(* Ziting: Same as get_all_tiles except for that no_shift returns the 
 * relative coordinates (aka map index) of left bottom corners, 
 * not absolute coordinates as in get_all_tiles *)
let get_all_tiles_no_shift room = 
  let tiles = ref [] in
  let map = room.map in
  let len = Array.length map in
  for x = 0 to len - 1 do 
    for y = 0 to len - 1 do 
      if cleanable_no_shift room (x,y)
      then tiles := (x,y) :: !tiles
    done;
  done;
  !tiles 

let get_tiles_num room =
  let tiles = get_all_tiles room in
  List.length tiles 

(* Is a neighbor tile reachable from the current tile?
 * 
 * Four next-door neighbors are always reachable given that 
 * they are a tile (cleanable)

 * A diagonal tile is reachable if:
 * 1. It is cleanable (aka. is a room tile) &&
 * 2. Its two neighbors in the same 2x2 square as the current tile 
 * are in the room/cleanable *)

let reachable room coor neighbor = 
  let (a, b) = coor in 
  let (c, d) = neighbor in
  let (dx, dy) = (c-a, d-b) in

  (* the neighbor is a tile *)
  cleanable room neighbor &&

  begin
    (*the neighbor is next-door -> reachable *)
    ((abs dx = 1 && dy = 0) || (dx = 0 && abs dy = 1)) ||
    (*the neighbor is diagonal to current tile*)
    begin 
    (cleanable room (a, d)) &&
    (cleanable room (c, b))
    end
  end


(* ========Tests======= *)

let%test "test_get_tiles_num" =
  let input  = BinaryEncodings.find_file "../../../resources/basic.txt" in
  let polygon_list = file_to_polygons input in
  let p = List.hd polygon_list in
  let room = polygon_to_room p in 
  let num = get_tiles_num room in
  num = 20

let%test "test_get_all_points" =
  let input  = BinaryEncodings.find_file "../../../resources/basic.txt" in
  let polygon_list = file_to_polygons input in
  let p = List.hd polygon_list in
  let room = polygon_to_room p in 
  let all_points = get_all_points room in
  let num = List.length all_points in 
  num = 81

let%test "test_get_all_tiles" = 
  let s = "(0, 0); (1, 0); (1, 1); (2, 1); (2, 2); (0, 2)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  let tiles = get_all_tiles room in
  List.for_all (fun x -> List.mem x tiles ) [(0, 0); (0, 1) ;(1, 1)] 

let%test "test_get_all_tiles_no_shift" = 
  let s = "(0, 0); (1, 0); (1, 1); (2, 1); (2, 2); (0, 2)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  let tiles = get_all_tiles_no_shift room in
  List.for_all (fun x -> List.mem x tiles ) [(0, 0); (0, 1) ;(1, 1)] 

let%test "test_reachable" = 
  let s = "(0, 0); (1, 0); (1, 1); (2, 1); (2, 2); (0, 2)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  reachable room (0, 0) (0, 1) &&
  not (reachable room (0, 0) (1, 1))

let%test "test_cleanable" = 
  let s = "(0, 0); (1, 0); (1, 1); (2, 1); (2, 2); (0, 2)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  cleanable room (0, 0) &&
  cleanable room (0, 1) &&
  cleanable room (1, 1) &&
  not (cleanable room (1,0)) &&
  not (cleanable room (2,1)) &&
  not (cleanable room (2,2))

let%test "test_cleanable 1x1 square" = 
  let s = "(0, 0); (1, 0); (1, 1); (0, 1)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  cleanable room (0, 0) &&
  not (cleanable room (1,0)) &&
  not (cleanable room (1,1)) &&
  not (cleanable room (0,1))

let%test "test_checker_simple 2x2 square" = 
  let s = "(0, 0); (2, 0); (2, 2); (0, 2)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  cleanable room (0, 0) &&
  cleanable room (1, 1) &&
  cleanable room (0, 1) &&
  cleanable room (1, 0) &&
  not (cleanable room (2, 0)) &&
  not (cleanable room (2, 2)) &&
  not (cleanable room (0, 2))




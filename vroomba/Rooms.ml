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

open ArrayUtil
open Polygons
open Util

(*********************************************)
(*         Representation of Rooms           *)
(*********************************************)

(* TODO: provide your implementation of internal room data type *)
(* It should describe the room conveniently for solving. *)
type pos =
  | Edge
  | Inner
  | Outer

type room = {
  map : (pos array) array;
  edges : (int * int) list ref;
  shift : (int * int) ref
}

let mk_room size =
  let map = Array.make size [||] in
  for i = 0 to size - 1 do
    map.(i) <- Array.make size Outer
  done;
  { map = map;
    edges = ref [];
    shift = ref (0, 0)
  }

(*  Read a polygon from a string of coordinates as in resources/basic.txt  *)
(*  A string can be ill-formed! *)

(* This function is flexible with diff formats, but may have difficulty 
 * catching some ill-formed strings, e.g. "(8-8, 0)" *)
let string_to_polygon (s : string) : polygon option =
  let extract_number string =
    let len = String.length string in
    let int_offset = int_of_char '0' in
    let is_neg = ref false in
    let num = ref 0 in
    let i = ref 0 in
    while !i < len do
      let ch = string.[!i] in
      if ch = ' ' then ()
      else if ch = '(' then ()
      else if ch = '-'
      then is_neg := true
      else if int_of_char ch >= int_offset &&
              int_of_char ch <= int_offset + 9
      then num := !num * 10 + ((int_of_char ch) - int_offset)
      else if !i = len - 1 && ch = ')' then ()
      else error "Unrecognizable index!";
      i := !i + 1
    done;
    if !is_neg
    then 0 - !num
    else !num
  in  
  let res = ref [] in
  let coords = String.split_on_char ';' s in
  try (
    List.map (fun e -> String.split_on_char ',' e) coords |>
    List.iter (fun ls ->
        if List.length ls = 2
        then begin
          let x = extract_number @@ List.hd ls in
          let y = extract_number @@ List.nth ls 1 in
          res := (x, y) :: !res
        end
        else error "Ill-formed string!");
    Some (polygon_of_int_pairs (List.rev !res)))
  with error -> None

(*  Read all polygons from a file  *)
let file_to_polygons (path: string) : polygon list =
  try (let ls = ReadingFiles.read_file_to_strings path in
       let res = ref [] in
       List.iter (fun e -> let poly = (string_to_polygon e) in
                   if poly = None then ()
                   else res := (get_exn poly) :: !res) ls;
       List.rev !res)
  with Sys_error _ -> error "No such file or directory!"

let polygon_to_string (p: polygon) : string =
  let buffer = Buffer.create 1 in
  List.iter (fun e ->
      let x = get_x e in
      let y = get_y e in
      let x_str = string_of_int (int_of_float x) in
      let y_str = string_of_int (int_of_float y) in
      let coord = String.concat "" ["("; x_str; ", "; y_str; "); "] in
      Buffer.add_string buffer coord) p;
  let s = Buffer.contents buffer in
  String.sub s 0 (String.length s - 2)

let write_polygons_to_file (ps: polygon list) (path: string) : unit =
  let res = ref [] in
  List.iter (fun p -> res := (polygon_to_string p) :: !res) ps;
  ReadingFiles.write_strings_to_file path (List.rev !res)

(*********************************************)
(*           Rooms and polygons              *)
(*********************************************)

let fill_edges map ls =
  let fill_edge x1 y1 x2 y2 =
    (* If there are diagonal edges, throw error *)
    if abs (x1 - x2) > 0 && abs (y1 - y2) > 0
    then error "Invalid room!"
    else if abs (x1 - x2) > 0
    then (if x1 > x2
          then (for i = 1 to (x1 - x2 - 1) do
                  map.(x2 + i).(y1) <- Edge
                done)
          else (for i = 1 to (x2 - x1 - 1) do
                  map.(x1 + i).(y1) <- Edge
                done))
    else if abs (y1 - y2) > 0
    then (if y1 > y2
          then (for i = 1 to (y1 - y2 - 1) do
                  map.(x1).(y2 + i) <- Edge
                done)
          else (for i = 1 to (y2 - y1 - 1) do
                  map.(x1).(y1 + i) <- Edge
                done))
  in
  let hd_init = List.hd ls in
  let tl_init = List.tl ls in
  let rec walk ls a =
    let (x1, y1) = a in
    map.(x1).(y1) <- Edge;
    match ls with
    | [] ->
      let (x2, y2) = hd_init in
      fill_edge x1 y1 x2 y2
    | (x2, y2) :: tl ->
      fill_edge x1 y1 x2 y2;
      walk tl (x2, y2)
  in walk tl_init hd_init


(* **************** ZITING'S ADDITION **************** *)
(* **************** From here onwards **************** *)

let coor_to_point (x,y)=
  Point (float_of_int x, float_of_int y)

let map_index_to_coor room (x,y)  = 
  let (minx, miny) = !(room.shift) in
  (x + minx, y + miny)

(* After the edges are already filled *)
let fill_room room map edges =
  let len = Array.length map in 
  let pol = polygon_of_int_pairs edges in 
  for i = 0 to len - 1 do
    for j = 0 to len - 1 do
      if map.(i).(j) <> Edge
      then begin
      (* test if the center is inside the polygon *)
      if point_within_polygon_2 pol (map_index_to_coor room (i, j)
      |> coor_to_point |> get_center)
      then map.(i).(j) <- Inner
      end
    done 
  done

(* **************** ZITING'S ADDITION **************** *)
(* **************** Till here **************** *)

(*  Convert a polygon to a room data type  *)
let polygon_to_room (p: polygon) : room =
  (* get information about the polygon *)
  let pos_x = ref 0 in
  let pos_y = ref 0 in
  let neg_x = ref 0 in
  let neg_y = ref 0 in
  let ls = ref [] in
  List.iter (fun e ->
      let x = int_of_float @@ get_x e in
      let y = int_of_float @@ get_y e in
      
      (* get room size with max/min coordinates *)
      if x > !pos_x then pos_x := x;
      if y > !pos_y then pos_y := y;
      
      (* and get shift values for negative coordinates *)
      if x < !neg_x then neg_x := x;
      if y < !neg_y then neg_y := y;

      (* get int coordinates of float pairs *)
      ls := (x, y) :: !ls) p;

  (* create room based on coordinate list *)
  let range_x = !pos_x - !neg_x in
  let range_y = !pos_y - !neg_y in
  let r = mk_room (1 + max range_x range_y) in
  r.edges := (List.rev !ls);
  if !neg_x < 0 || !neg_y < 0
  then begin
    let ls' = List.map (fun (x, y) -> (x - !neg_x, y - !neg_y)) !ls in
    fill_edges r.map ls';
    r.shift := (!neg_x, !neg_y)
  end
  else fill_edges r.map !(r.edges);
  fill_room r r.map !(r.edges);
  r

(*  Convert a room to a list of polygon coordinates  *)
let room_to_polygon (r: room) : polygon = 
  polygon_of_int_pairs !(r.edges)


(* Tests *)

let%test "test file_to_polygon & write_polygons_to_file" = 
  let input  = BinaryEncodings.find_file "../../../resources/rooms.txt" in
  let output = "test.tmp" in
  let string = ReadingFiles.read_file_to_strings input in 
  let polygon_list = file_to_polygons input in
  write_polygons_to_file polygon_list output;
  let string' = ReadingFiles.read_file_to_strings output in
  Sys.remove output;
  string = string'

let%test "test file_to_polygon & write_polygons_to_file random" = 
  let input  = BinaryEncodings.find_file "../../../resources/test_generate_l.txt" in
  let output = "test.tmp" in
  let string = ReadingFiles.read_file_to_strings input in 
  let polygon_list = file_to_polygons input in
  write_polygons_to_file polygon_list output;
  let string' = ReadingFiles.read_file_to_strings output in
  Sys.remove output;
  string = string'

let%test "test polygon_to_room & room_to_polygon 1" = 
  let input  = BinaryEncodings.find_file "../../../resources/basic.txt" in
  let polygon_list = file_to_polygons input in
  List.for_all (fun p -> 
                    let room = polygon_to_room p in 
                    let p' = room_to_polygon room in 
                    p = p') 
  polygon_list

let%test "test polygon_to_room & room_to_polygon 2" = 
  let input  = BinaryEncodings.find_file "../../../resources/rooms.txt" in
  let polygon_list = file_to_polygons input in
  List.for_all (fun p -> 
      let room = polygon_to_room p in 
      let p' = room_to_polygon room in 
      p = p') 
    polygon_list

let%test "test polygon_to_room & room_to_polygon 3" = 
  let input  = BinaryEncodings.find_file "../../../resources/test_generate_l.txt" in
  let polygon_list = file_to_polygons input in
  List.for_all (fun p -> 
      let room = polygon_to_room p in 
      let p' = room_to_polygon room in 
      p = p') 
    polygon_list

let%test "test polygon_to_room & room_to_polygon negative" = 
  let input  = BinaryEncodings.find_file "../../../resources/invalid.txt" in
  let polygon_list = file_to_polygons input in
  List.for_all (fun p -> 
                    try
                    let room = polygon_to_room p in 
                    let p' = room_to_polygon room in 
                    p = p'
                    with Failure _ -> true) 
  polygon_list


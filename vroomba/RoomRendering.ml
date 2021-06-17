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

include Util
open ArrayUtil
open Rooms
open RoomChecker
open RoomGenerator
open RoomUtil
open GraphicUtil
open ReadingFiles
include Polygons

(*********************************************)
(*           Gamifying the solver            *)
(*********************************************)


(* Helper functions *)

let fill_poly_color ?color:(color = Graphics.black) poly_list = 
  set_color color;
  fill_poly poly_list

let print_tuple (x,y) =
  Printf.printf "(%d, %d)\n" x y 


(* There's probably a better way of doing this but we're short 
 * on time and unfamiliar with Core functions... so *)
let write_solution_to_file_appendable moves path =
  let init =
    try (BinaryEncodings.read_file_to_single_string path)
    with Failure _ -> "" in
  let buffer = Buffer.create 1 in
  List.iter (fun e -> Buffer.add_string buffer (pp_move e)) moves;
  let new_string = Buffer.contents buffer in
  let full =
    if init = ""
    then new_string
    else init ^ "\n" ^ new_string in
  ReadingFiles.write_string_to_file path full
 
(* Top-level funciton *)
(* let render_games_eg2 (input_path: string) (output_path : string): unit =  *)
let render_games (input_path: string) (output_path : string) = 

  ReadingFiles.write_string_to_file output_path "";
  
  (* a bit costly to render all polygons as rooms at the very first, 
   * instead of at each room, so we make the functions take the array
   * of polygons and the index we're at in the array *)
  let poly_arr = file_to_polygons input_path |> list_to_array in
                  
  (* ***************************** GRAPHICS *****************************  *)
  
  let get_abs (ox,oy) t_width (x,y) = 
    ox + t_width * x, oy + t_width * y

  in let get_abs_from_coor r (ox,oy) t_width (x,y) =
       (x,y) |> coor_to_map_index r |> get_abs (ox, oy) t_width

  in let draw_board r =
       let wrapper_dim = (800, 800) in
       let board_dim = (600, 600) in
       open_graph @@ Printf.sprintf " %dx%d" (fst wrapper_dim) (snd wrapper_dim);
       let lbc_board = ((fst wrapper_dim - fst board_dim) / 2, 
                        (snd wrapper_dim - snd board_dim) / 2) in
       set_color (rgb 255 255 255);
       fill_rect (fst lbc_board) (snd lbc_board) (fst board_dim) (snd board_dim);

       let tile_width = (fst board_dim) / (Array.length r.map) in
       lbc_board, tile_width


  in let draw_room r lbc_board tile_width =
       (* Fill the room *)  
       let room_int_pairs_array_abs = get_edges_no_shift r |> list_to_array 
                                      |> Array.map (get_abs lbc_board tile_width) in
       fill_poly_color ~color:(rgb 100 100 100) room_int_pairs_array_abs

  in let draw_clean tile_width (x,y) = 
       set_color (rgb 209 237 242);
       fill_rect x y tile_width tile_width 

  in let display_vroomba tile_width (x,y) =
       set_color Graphics.green;
       fill_rect x y tile_width tile_width

in let clean_and_draw r state coor lbc_board tile_width =
       (* clean and draw vroomba position *)
       let abs = get_abs_from_coor r lbc_board tile_width coor in
       display_vroomba tile_width abs; 
       clean_a_tile state coor;

       (* clean surrounding tiles *)
       let neighbors = get_eight_neighbors coor in
       List.iter (fun n -> 
           let n_abs = get_abs_from_coor r lbc_board tile_width n in
           if is_a_tile state n
           then 
             (if reachable r coor n
              then (clean_a_tile state n;
                    draw_clean tile_width n_abs))
         ) neighbors;

  in let print_instructions cleaned_num room_num =
       set_color Graphics.black;
       moveto 200 750;
       draw_string "Move the green Vroomba to clean the room! Cleaned tiles are blue.";
       moveto 200 730;
       draw_string "Press 'W', 'S', 'A', 'D' to go up, down, left and right";
       moveto 150 70;
       draw_string "When the current room is fully clean, you will automatically proceed to the next room";
       moveto 200 50;
       draw_string "Press 'Q' to save solutions and exit the game at any point";     
       moveto 620 730;
       let str = Printf.sprintf "Rooms Cleaned: %d / %d" cleaned_num room_num in
       draw_string str
  
  in let finish _ = 
      open_graph " 800x800";
       set_color Graphics.black;
       moveto 300 400;
       draw_string "You have cleaned all the rooms. Well done!";
       moveto 300 370;
       draw_string "Press 'Q' to exit.";
      let event = wait_next_event [Key_pressed] in
      if event.key == 'q'
      then close_graph ()



  (* ***************************** GRAPHICS *****************************  *)


  (* ***************************** Keyboard input **************************  *)


  (* Ask for user input. If the user requests to go to a tile, update the display 
     of the Vroomba tile. Clean the neighbouring tiles of the next tile and
     display them as clean.

     Record the move. Check whether the room is finished, and if it is,
     check for the user input to proceed with the next room.

     If the move isn't valid (it's not a tile), 
     there is no display update. Don't record the move. *)

  in let rec wait_until_q_pressed r state move_list lbc_board tile_width p_arr i =

       if !(state.dirty_tiles) = 0
       then (write_solution_to_file_appendable
               (List.rev move_list) output_path;
             close_graph ();
             play p_arr (i + 1))
       else begin
         let curr_coor = !(state.current) in

         (* Ask for user input *)
         let event = wait_next_event [Key_pressed] in
         if event.key == 'q'
         then (write_solution_to_file_appendable
                 (List.rev move_list) output_path;
               close_graph ())
         else if List.mem event.key ['a'; 's'; 'd'; 'w']
         then begin
           let m = match event.key with 
             | 'w' -> RoomChecker.Up
             | 'a' -> Left
             | 's' -> Down
             | 'd' -> Right
             | _ -> error "Unrecognizable move!" (* won't reach this step *)
           in let next_coor = move_in_dir curr_coor m in
           if not (is_a_tile state next_coor)
           then wait_until_q_pressed r state
               move_list lbc_board tile_width p_arr i
           else begin
             (* Update the current position of the state *)
             state.current := next_coor;

             (* Clean the tile and the neighbouring tiles 
              * Then reflect the cleaning on the renderng *)
             clean_and_draw r state next_coor lbc_board tile_width;

             let move_list' = (m :: move_list) in
             wait_until_q_pressed r state
               move_list' lbc_board tile_width p_arr i

           end
         end

         (* Other keys *)
         else wait_until_q_pressed r state
             move_list lbc_board tile_width p_arr i
       end



  (* ***************************** Keyboard input **************************  *)
           

  and play p_arr i = 
    (* Initialize board and room rendering. 
       Initialize state.
       Display Vroomba at the initial tile *)
    if i < Array.length p_arr
    then begin
      let p = p_arr.(i) in
      let r = polygon_to_room p in

      let (lbc_board, tile_width) = draw_board r in
      draw_room r lbc_board tile_width;

      (* print instructions on interface *)
      print_instructions (i) (Array.length p_arr);

      let state = initiate_state r in 
      let starting_coor = !(state.current) in
      
      (* Clean the first tile and its neighboring tiles *)
      clean_and_draw r state starting_coor lbc_board tile_width;

      wait_until_q_pressed r state [] lbc_board tile_width p_arr i
    end

    else finish ()
    
  in play poly_arr 0

(* testing for ourselves *)
let try_render input =
  let f = BinaryEncodings.find_file "resources/" ^ input ^ ".txt" in 
  render_games f "resources/test.sol"

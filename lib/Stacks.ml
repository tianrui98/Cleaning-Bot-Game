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

include ArrayUtil

(***********************************************)
(*             Representing stacks             *)
(***********************************************)

module type AbstractStack = sig
    type 'e t
    val mk_stack : int -> 'e t
    val is_empty : 'e t -> bool
    val push : 'e t -> 'e -> unit
    val pop : 'e t -> 'e option
  end

(* Simple stack based on OCaml lists *)

module ListBasedStack : AbstractStack = struct
    type 'e t = 'e list ref
    let mk_stack _ = ref []
    let is_empty s = match !s with
      | [] -> true
      | _ -> false
    let push s e = 
      let c = !s in
      s := e :: c
    let pop s = match !s with
      | h :: t ->
        s := t; Some h
      | _ -> None
  end

(* Array-based stack *)

module ArrayBasedStack : AbstractStack = struct

  type 'e t = {
      elems   : 'e option array;
      cur_pos : int ref 
    }

  let mk_stack n = {
      elems = Array.make n None;
      cur_pos = ref 0
    }
 
 let is_empty s = !(s.cur_pos) = 0

 let push s e = 
   let pos = !(s.cur_pos) in 
   if pos >= Array.length s.elems 
   then raise (Failure "Stack is full")
   else (s.elems.(pos) <- Some e;
         s.cur_pos := pos + 1)
      
    let pop s = 
      let pos = !(s.cur_pos) in
      let elems = s.elems in
      if pos <= 0 then None
      else begin
        let res = elems.(pos - 1) in
        s.elems.(pos - 1) <- None;
        s.cur_pos := pos - 1;
        res
        end
end


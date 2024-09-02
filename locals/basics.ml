(* Jane Street has added *locals* to OCaml. Because local values are
   not allowed to escape their allocating region (e.g. function body),
   we can deallocate them without garbage collection.

   This speeds up our programs.

   This file shows some of the aspect of how the types work; the file
   stack.ml is a demonstration of how stack allocation can actually
   save runtime garbage-collected allocations. *)

(* [f1] here doesn't work (try it!) because it has an escaping local: *)

(*
let f1 () =
  let x @ local = "hello" in
  x
*)

(* Changing the "local" there to "global" (or omitting the mode annotation
   entirely) fixes the problem. *)

(* Globals cannot refer to locals: allowing that would allow you
   to smuggle a local out from a function. *)

(* Thus f2 here doesn't work (try it!): *)

(*
let f2 () =
  let x @ local = "hello" in
  let _xs @ global = [x] in
  ()
*)

(* On the other hand, storing a global in a local is just fine: *)

let f3 () =
  let x @ global = "hello" in
  let _xs @ local = [x] in
  ()

(* The "no escape" property of locals allows us to write a safe
   [with_file] construct: *)

module With_file : sig
  type file_handle
  val with_file : filename:string -> f:(file_handle @ local -> 'r @ global) -> 'r @ global

  (* The following [@ local] annotation is necessary to say that
     [read_file] promises not to save the handle anywhere. *)
  val read_file : file_handle @ local -> string
end = struct
  type file_handle
  let with_file ~filename:_ ~f:_ = failwith "unimplemented"
  let read_file _handle = failwith "unimplemented"
end

(* This access pattern is OK: *)
let f4 () =
  let open With_file in
  with_file ~filename:"data.txt" ~f:(fun handle ->
      let contents = read_file handle in
      ignore contents)

(* This access pattern is not (try it!): *)

(*
let f5 () =
  let open With_file in
  let handle_ref = ref None in
  with_file ~filename:"data.txt" ~f:(fun handle ->
      (* store it in the ref: *)
      handle_ref := Some handle);
  !handle_ref
*)

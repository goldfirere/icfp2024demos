(* We want to be able to track *uniqueness* in order to do in-place
   memory update of immutable fields. A *unique* pointer is not aliased:
   no other live pointer points to the same value as a unique pointer. *)

(* Compile this file with
     > ocamlc -extension-universe alpha -i basics.ml
*)

type 'a box = { contents : 'a }

let f1 : unit -> string box @ unique = fun () ->
  { contents = "hello" }

(* This one doesn't work, because the returned pointer isn't unique. *)
let global_ref = ref { contents = "default" }
(*
let f2 : unit -> string box @ unique = fun () ->
  let x = { contents = "hello" } in
  global_ref := x;
  x
*)

(* A function that does in-place update of an immutable field
   can be run only once: running it twice will unexpectedly edit
   the same memory region. *)

(* We don't have in-place update working, so use this instead, to
   assert uniqueness: *)
let require_unique : 'a @ unique -> 'a @ unique = fun x -> x

(* When you compile this file with the [-i] flag, you'll see the
   inferred types for all the definitions. Notice that the returned
   function from [f3] is labeled [once]: this denotes that the
   function can be called only once. *)
let f3 () =
  let x @ unique = { contents = "hello" } in
  fun () -> require_unique x

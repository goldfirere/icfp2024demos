(* Jane Street's version of OCaml tracks the *portability* of
   values. Portable values can be safely sent across threads,
   without introducing the possibility of data races.

   Data races can arise only from mutability. So a more
   primitive construct is *contention*. No one can access
   (read or write) mutable fields in a contended value. *)

(* Compile this file with
     > ocamlc -extension-universe alpha -allow-illegal-crossing -i basics.ml
   Ask us about what these extra flags mean! *)

(* This works fine: *)
let f1 () =
  let r @ uncontended = ref "hello" in
  r := "goodbye";
  !r

(* This does not (try it!): *)
(*
let f2 () =
  let r @ contended = ref "hello" in
  r := "goodbye";
  !r
*)

(* Here is a very simple API that uses contention and portability: *)
module Fork_join : sig
  val in_parallel : (unit -> 'a) @ portable -> (unit -> 'b) @ portable -> 'a * 'b @@ portable
end = struct
  let in_parallel f1 f2 = f1 (), f2 ()  (* toy implementation *)
end

(* Here is a silly example of using that API: *)
let rec fib n =
  (* Must redefine library functions to be known to be portable: *)
  let open struct
        external (+) : int -> int -> int @@ portable = "%addint"
        external (-) : int -> int -> int @@ portable = "%subint"
        external (<=) : int -> int -> bool @@ portable = "%lessequal"
      end in
  if n <= 1 then n else
  let f_minus_1, f_minus_2 = Fork_join.in_parallel (fun () -> fib (n-1)) (fun () -> fib (n-2)) in
  f_minus_1 + f_minus_2

(* What might not be portable? *)
let sneaky () =
  let r @ uncontended = ref "hello" in
  (fun () -> r := "goodbye"), (fun () -> !r)

(* Imagine applying Fork_join.in_parallel to the two result functions of [sneaky].
   The result would be a race. So we don't allow it (try it!): *)
(*
let racy () =
  let f1, f2 = sneaky () in
  Fork_join.in_parallel f1 f2
*)

(* The error complains (rightly!) that [f1] and [f2] are nonportable. Why? Because
   they close over an uncontended [r]. Functions that do mutation *can* be portable,
   as long as the mutable data is local to the function. This is made more explicit
   by the [@ uncontended] in the definition of [sneaky]. (That annotation is optional.) *)

(* Here is an API that uses [contended], where values that have moved across
   threads come out [contended]; see the return value from [read_from]. *)
module Messaging : sig
  type -'a write_pipe
  type +'a read_pipe

  (* Below, all the [@@ portable] annotations are saying that these functions
     are safe to transmit across threads. *)

  (* Fork a new thread running a process; that process can send messages back
     to the originating thread. *)
  val fork : ('a write_pipe @ local -> unit) @ portable -> 'a read_pipe @@ portable

  (* Pass information back to the originating thread. *)
  val pass_back : 'a @ portable -> 'a write_pipe @ local -> unit @@ portable

  (* Block until a message is available on the pipe and return it. *)
  val read_from : 'a read_pipe -> 'a @ contended @@ portable
end = struct
  type -'a write_pipe
  type +'a read_pipe

  (* We must redefine [failwith] to be portable. *)
  external failwith : string -> 'a @@ portable = "failwith"
  let fork _f = failwith "unimplemented"
  let pass_back _data _pipe = failwith "unimplemented"
  let read_from _pipe = failwith "unimplemented"
end

(* A linked list containing mutable integers.
   The [value mod portable] annotation allows values of this
   type to be portable, even when a field is mutable. This
   annotation won't be necessary in our final design. *)
type intlist : value mod portable =
  { mutable data : int
  ; rest : intlist option }

let rec length { rest } = match rest with
  | None -> 0
  | Some list -> 1 + length list

let messaging_example () =
  let open Messaging in
  let read_pipe =
    fork (fun write_pipe ->
        let list : intlist = { data = 1; rest = Some { data = 2; rest = None }} in
        pass_back list write_pipe;
        list.data <- 10)
  in
  let transported_list = read_from read_pipe in
  (* This is OK: the length doesn't depend on mutable data. *)
  length transported_list
  (* This is not: we can't read mutable data from contended values *)
  (* , transported_list.data *)

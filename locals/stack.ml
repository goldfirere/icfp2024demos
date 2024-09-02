(* Look at [basics.ml] before looking here. *)

(* Because local values do not escape their function, we can use
   a stack allocation discipline to back deallocating local
   values independent of the garbage collector. *)

(* This file defines a program that generates a matrix filled
   with random integers. It then multiplies each integer in the
   matrix with the index of its row. That way, when we print, we
   can easily see whether the operation happened (because all the
   numbers will be multiples of the row index). Note that doing
   this operation requires nested iterations, where the inner
   iteration makes a closure that captures the row index.

   If we use reimplementations of the stdlib Array functions that
   note their function argument as [local], then we know that the
   functions passed to [iter] and [iteri] can be stack allocated.

   Compile this program (with
     > ocamlopt -o stack stack.ml
   ) and then run it, producing GC statistics (with
     > OCAMLRUNPARAM="v=0x400" ./stack
   ). Observe the difference bwtween using the stdlib array functions
   and our localized equivalents. *)

let size = 20

module Local = struct
  module Array = struct
    let init size (f @ local) =
      if size = 0 then [| |] else
      let first = f 0 in
      let arr = Array.make size first in
      for index = 1 to size - 1 do
        arr.(index) <- f index
      done;
      arr

    let iteri (f @ local) arr =
      for index = 0 to Array.length arr - 1 do
        f index arr.(index)
      done

    (* The [@nontail[ annotation here is a tricky bit. Come talk to
       us about it if you're interested! *)
    let iter (f @ local) arr = iteri (fun _index -> f) arr [@nontail]

    (* These are necessary because the syntax [a.(i)] is really just
       a call to [Array.get a i] for whatever [Array] module is in scope,
       and similarly for [set]. *)
    let get = Array.get
    let set = Array.set
  end
end

let () =
  (* Uncomment this next line: *)
  (* let open Local in *)
  Random.init 1;
  let arr =
    Array.init size (fun _index ->
        Array.init size (fun _index ->
            Random.int 100))
  in
  Array.iteri (fun r inner ->
      Array.iteri (fun c elt -> arr.(r).(c) <- elt * r) inner [@nontail]) arr;
  Array.iteri (fun r inner ->
      Printf.printf "%d: " r;
      Array.iter (fun elt -> Printf.printf "%d " elt) inner;
      Printf.printf "\n") arr

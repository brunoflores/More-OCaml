module Test : sig
  type t

  val name : t -> string
  val create : name:string -> (unit -> unit -> unit) -> t
end

val run : Test.t list -> unit

(* Helpers *)
val ints : int -> int list

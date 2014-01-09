type instr =
  | Not_deeper of int
  | Stop
  | Continue

type 'a t
type rect

val from_rectangle : Rectangle.i -> rect
val rect : int -> int -> int -> int -> rect

val create : ?bound:rect -> ?depth:int -> int -> 'a t
val copy : 'a t -> 'a t
val clear : 'a t -> unit

val find : 'a t -> rect -> ('b -> rect -> 'a -> 'b * instr) -> 'b -> 'b
val to_list : 'a t -> (rect * 'a) list
val fold : ('b -> (rect * 'a) -> 'b) -> 'b -> 'a t -> 'b

val insert : 'a t -> rect -> 'a -> unit
val clean_insert : 'a t -> rect -> 'a -> 'a t

val draw : Sdlvideo.surface -> 'a t -> unit

(*
val insert_tailrec : 'a t -> int * int -> 'a -> unit

val remove : 'a t -> int * int -> 'a -> unit

val depth : 'a t -> int
*)

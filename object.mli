type t

val new_object : int -> int -> int -> int -> t

val update_pos : (int * int) -> t -> t
val update_dim : (int * int) -> t -> t

val to_rect : t -> Sdlvideo.rect

val get_x : t -> int
val get_y : t -> int
val get_w : t -> int
val get_h : t -> int

val update : t -> t

val inter : t -> t -> bool

val print : t -> unit

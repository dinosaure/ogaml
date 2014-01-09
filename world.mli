type 'a t = {
  event: int Event.t;
  screen: Sdlvideo.surface;
  quadtree: 'a Qt.t;
}

val new_world : int -> int -> 'a t
val add_object : (< to_rect: Rectangle.i; .. > as 'a) -> 'a t -> 'a t

val update : (< to_rect: Rectangle.i; update: 'a Update.f; .. > as 'a) t -> 'a t
val draw : (< to_draw: Draw.i; .. > as 'a) t -> unit

val get_screen : 'a t -> Sdlvideo.surface
val get_quadtree : 'a t -> 'a Qt.t

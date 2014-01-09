type f = Sdlvideo.surface -> unit

class type virtual i =
  object ('a)
    method virtual draw : (Sdlvideo.surface -> unit)
    method to_draw : i
  end

class virtual t =
  object (s)
    method virtual draw : (Sdlvideo.surface -> unit)
    method to_draw = (s :> i)
  end

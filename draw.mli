type f = Sdlvideo.surface -> unit

class type virtual i =
  object ('a)
    method virtual draw : f
    method to_draw : i
  end

class virtual t :
  object ('a)
    method virtual draw : f
    method to_draw : i
  end

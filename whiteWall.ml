module SV = Sdlvideo

class t x y w h =
  object (s : 'a)
    inherit Rtype.t Rtype.WhiteWall
    inherit Rectangle.t x y w h
    inherit Update.t
    inherit Draw.t

    method update : 'a Update.f =
      (fun _ _ -> {< >})
    method draw screen =
      SV.fill_rect ~rect:(SV.rect x y w h) screen
       (SV.map_RGB screen (0xFF, 0xFF, 0xFF))
end

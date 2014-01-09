class t :
  int -> int -> int -> int ->
  object ('a)
    inherit Rectangle.t
    inherit Update.t
    inherit Draw.t
    inherit Rtype.t

    method update : 'a Update.f
    method draw : Draw.f
  end

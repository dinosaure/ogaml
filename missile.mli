class t :
  int -> int -> (int * int) -> int ->
  object ('a)
    inherit Rtype.t
    inherit Rectangle.t
    inherit Update.t
    inherit Draw.t

    method update : 'a Update.f
    method draw : Draw.f
  end

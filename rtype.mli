type o =
  | Player
  | WhiteWall
  | Missile

class type i =
  object ('a)
    val o : o

    method get_type : o
  end

class t :
  o ->
  object ('a)
    val o : o

    method get_type : o
  end

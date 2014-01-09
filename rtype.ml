type o =
  | Player
  | WhiteWall
  | Missile

class type i =
  object
    val o : o

    method get_type : o
  end

class t (o' : o) =
  object
    val o = o'

    method get_type = o
  end

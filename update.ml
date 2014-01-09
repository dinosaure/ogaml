type 'a f = (int Event.t -> 'a Qt.t -> 'a)

class type virtual i =
  object ('a)
    method virtual update : 'a f
  end

class virtual t =
  object (s : 'a)
    method virtual update : 'a f
  end


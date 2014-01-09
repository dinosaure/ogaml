class type i =
  object ('a)
    val x : int
    val y : int
    val w : int
    val h : int

    method get_x : int
    method get_y : int
    method get_w : int
    method get_h : int

    method to_rect : i

    method move : (int * int) -> 'a
    method resize : (int * int) -> 'a
  end

class t x' y' w' h' =
  object (s)
    val x = x'
    val y = y'
    val w = w'
    val h = h'

    method get_x = x
    method get_y = y
    method get_w = w
    method get_h = h

    method to_rect = (s :> i)

    method move (x', y') =
      {< x = x + x'; y = y + y' >}
    method resize (w', h') =
      {< w = w + w'; h = h + h' >}
  end

let inter a b =
  let x1, x2, y1, y2 = a#get_x, a#get_x + a#get_w, a#get_y, a#get_y + a#get_h in
  let x1', x2', y1', y2' = b#get_x, b#get_x + b#get_w, b#get_y, b#get_y + b#get_h in
  x1 < x2' && x2 > x1' && y1 < y2' && y2 > y1'

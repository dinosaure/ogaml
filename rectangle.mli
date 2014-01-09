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

class t :
  int -> int -> int -> int ->
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

val inter: i -> i -> bool

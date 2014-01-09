module SV = Sdlvideo

type t = (int * int * int * int)

let new_object x y w h = (x, y, w, h)

let update_pos (xver, yver) (x, y, w, h) = (x + xver, y + yver, w, h)
let update_dim (wnew, hnew) (x, y, w, h) = (x, y, wnew, hnew)

let to_rect (x, y, w, h) = SV.rect x y w h

let get_x (x, _, _, _) = x
let get_y (_, y, _, _) = y
let get_w (_, _, w, _) = w
let get_h (_, _, _, h) = h

let update a = a

let inter (x, y, w, h) (x', y', w', h') =
  let x1, x2, y1, y2 = x, x + w, y, y + h in
  let x1', x2', y1', y2' = x', x' + w', y', y' + h' in
  x1 < x2' && x2 > x1' && y1 < y2' && y2 > y1'

let print (x, y, h, w) =
  Printf.printf "[%d,%d|%d,%d]\n%!" x y w h

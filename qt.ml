(* thx besport <3 *)

module SV = Sdlvideo
module SG = Sdlgfx

type instr =
  | Not_deeper of int
  | Stop
  | Continue

type rect = (int * int * int * int)

type 'a tree =
  | Node of ('a tree * 'a tree * 'a tree * 'a tree)
  | Leaf of int * (rect * 'a) list

type 'a t = {
  tree : 'a tree ref;
  bound : rect;
  slice : int;
  depth : int;
}

let from_rectangle r =
  (r#get_x, r#get_x + r#get_w, r#get_y, r#get_y + r#get_y)

let rect x1 x2 y1 y2 = (x1, x2, y1, y2)

let create ?(bound = (0, 0, 320, 200)) ?(depth = 15) slice = {
  tree = ref (Leaf (0, []));
  bound = bound;
  slice = slice;
  depth = depth;
}

let copy { bound; slice; depth; _ } = {
  tree = ref (Leaf (0, []));
  bound;
  slice;
  depth;
}

let clear t = t.tree := Leaf (0, [])

let rec up limit = function
  | [] -> []
  | ((x1, x2, _, _), _) :: nxt when x2 - x1 <= limit -> up limit nxt
  | nxt -> nxt

let find t (x1', x2', y1', y2') f acc =
  let inter (x1, x2, y1, y2) = x2 < x1' || y2 < y1' || x2' < x1 || y2' < y1 in
  let rec loop (x1, x2, y1, y2) acc nxt = function
    | Leaf (_, l) ->
      let rec aux acc = function
        | [] -> (function [] -> acc, None | (pos, t) :: nxt -> acc, Some (pos, nxt, t)) nxt
        | (_, e) :: xs ->
          begin match f acc (x1, x2, y1, y2) e with
            | acc, Continue -> aux acc xs
            | acc, Not_deeper limit ->
                (function [] -> acc, None | (pos, t) :: nxt -> acc, Some (pos, nxt, t)) (up limit nxt)
            | acc, Stop -> acc, None
          end
      in (function acc, None -> acc | acc, Some (pos, nxt, t) -> loop pos acc nxt t) (aux acc l)
    | Node (t1, t2, t3, t4) ->
      let x3 = (x1 + x2) / 2 in
      let y3 = (y1 + y2) / 2 in
      let area1 = (x1, x3, y1, y3) in
      let area2 = (x3, x2, y1, y3) in
      let area3 = (x1, x3, y3, y2) in
      let area4 = (x3, x2, y3, y2) in
      match inter area1, inter area2, inter area3, inter area4 with
        | false, f2, f3, f4 ->
          loop area1 acc
            (List.fold_left
              (fun acc (b, area, t) -> if not b then (area, t) :: acc else acc) nxt 
              [f2, area2, t2; f3, area3, t3; f4, area4, t4]) t1
        | true, false, f3, f4 ->
          loop area2 acc
            (List.fold_left
              (fun acc (b, area, t) -> if not b then (area, t) :: acc else acc) nxt 
              [f3, area3, t3; f4, area4, t4]) t2
        | true, true, false, f4 ->
          loop area3 acc
            (List.fold_left
              (fun acc (b, area, t) -> if not b then (area, t) :: acc else acc) nxt 
              [f4, area4, t4]) t3
        | true, true, true, false -> loop area4 acc nxt t4
        | true, true, true, true ->
          (function [] -> acc | (pos, t) :: nxt -> loop pos acc nxt t) nxt
  in let pos = t.bound in loop pos acc [] !(t.tree)

let fold' f a t =
  let rec aux a = function
    | Leaf (_, l) -> List.fold_left f a l
    | Node (t1, t2, t3, t4) ->
      let a1 = aux a t1 in
      let a2 = aux a1 t2 in
      let a3 = aux a2 t3 in
      aux a3 t4
  in aux a !(t.tree)

let to_list t = fold'
  (fun acc (r, x) -> if List.exists (fun (_, a) -> a = x) acc then acc else (r, x) :: acc) [] t

let fold f a t = List.fold_left f a (to_list t)

let insert tt ((x1', x2', y1', y2') as rect') e =
  let cons l = (rect', e) :: l in
  let inter (x1, x2, y1, y2) (x1', x2', y1', y2') = x2 < x1' || y2 < y1' || x2' < x1 || y2' < y1 in
  let rec loop x1 x2 y1 y2 t max = match t with
    | Leaf (size, l) when size < tt.slice || tt.depth < max -> Leaf (size + 1, cons l)
    | Leaf (_, l) ->
      let x3 = (x1 + x2) / 2 in
      let y3 = (y1 + y2) / 2 in
      let area1 = (x1, x3, y1, y3) in
      let area2 = (x3, x2, y1, y3) in
      let area3 = (x1, x3, y3, y2) in
      let area4 = (x3, x2, y3, y2) in
      let (l1, l2, l3, l4) =
        List.fold_left (fun (l1, l2, l3, l4) ((rect, e) as elt) ->
          (if not (inter area1 rect) then (elt :: l1) else l1),
          (if not (inter area2 rect) then (elt :: l2) else l2),
          (if not (inter area3 rect) then (elt :: l3) else l3),
          (if not (inter area4 rect) then (elt :: l4) else l4))
        ([], [], [], []) l in
      let t1, t2, t3, t4 =
        (Leaf (List.length l1, l1)),
        (Leaf (List.length l2, l2)),
        (Leaf (List.length l3, l3)),
        (Leaf (List.length l4, l4)) in
      Node (
        (if not (inter area1 rect') then loop x1 x3 y1 y3 t1 (succ max) else t1),
        (if not (inter area2 rect') then loop x3 x2 y1 y3 t2 (succ max) else t2),
        (if not (inter area3 rect') then loop x1 x3 y3 y2 t3 (succ max) else t3),
        (if not (inter area4 rect') then loop x3 x2 y3 y2 t4 (succ max) else t4))
    | Node (t1, t2, t3, t4) ->
      let x3 = (x1 + x2) / 2 in
      let y3 = (y1 + y2) / 2 in
      let area1 = (x1, x3, y1, y3) in
      let area2 = (x3, x2, y1, y3) in
      let area3 = (x1, x3, y3, y2) in
      let area4 = (x3, x2, y3, y2) in
      Node (
        (if not (inter area1 rect') then loop x1 x3 y1 y3 t1 (succ max) else t1),
        (if not (inter area2 rect') then loop x3 x2 y1 y3 t2 (succ max) else t2),
        (if not (inter area3 rect') then loop x1 x3 y3 y2 t3 (succ max) else t3),
        (if not (inter area4 rect') then loop x3 x2 y3 y2 t4 (succ max) else t4))
  in let x1, x2, y1, y2 = tt.bound in
  tt.tree := loop x1 x2 y1 y2 !(tt.tree) 0

let clean_insert tt rect e =
  insert tt rect e; tt

let draw screen tt =
  let rec aux (x1, x2, y1, y2) t =
    let draw_area ((x1, x2, y1, y2) as area) t =
      ignore (SG.rectangleRGBA screen
        (SV.rect x1 y1 x1 y1)
        (SV.rect x2 y2 x2 y2)
        (0x00, 0x00, 0x00) 0xFF);
      aux area t
    in match t with
      | Node (t1, t2, t3, t4) ->
        let x3 = (x1 + x2) / 2 in
        let y3 = (y1 + y2) / 2 in
        let area1 = (x1, x3, y1, y3) in
        let area2 = (x3, x2, y1, y3) in
        let area3 = (x1, x3, y3, y2) in
        let area4 = (x3, x2, y3, y2) in
          draw_area area1 t1;
          draw_area area2 t2;
          draw_area area3 t3;
          draw_area area4 t4
      | Leaf _ -> ()
  in aux tt.bound !(tt.tree)

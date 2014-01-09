type instr =
  | Not_deeper of int
  | Stop
  | Continue

type 'a tree = N of 'a tree * 'a tree * 'a tree * 'a tree | C of int * ((int*int)*'a) list
type 'a t = {
  tree : 'a tree ref;
  bound : int * int * int * int;
  slice : int;
  depth : int;
}

type rect = (int * int * int * int)

let init ?(bound = (0, 0, 320, 200)) ?(depth = 15) slice = {
  tree = ref (C (0,[]));
  bound = bound;
  slice = slice;
  depth = depth;
}

let clear t =
  t.tree := C (0,[])

let rec up limit = function
  | [] -> []
  | ((lat1, lat2, _, _), _) :: nxt when lat2 - lat1 <= limit -> up limit nxt
  | nxt -> nxt

let to_rect o =
  (Object.get_x o, Object.get_x o + Object.get_w o, Object.get_y o, Object.get_y o + Object.get_h o)

let fold t (lat1', lat2', lon1', lon2') f acc =
  let inter (lat1, lat2, lon1, lon2) = lon2 < lon1' || lat2 < lat1' || lon2' < lon1 || lat2' < lat1 in
  let rec loop (lat1, lat2, lon1, lon2) acc nxt t =
    match t with
      | C (_, l) ->
         let rec aux acc = function
         | [] ->
                (match nxt with
                 | [] -> acc,None
                 | (coord, t)::nxt -> acc, Some (coord, nxt, t))
         | ((y, x), e) :: xs ->
                match if lon1' < x && x < lon2' && lat1' < y && y < lat2' then f acc (lat1, lat2, lon1, lon2) e else acc, Continue with
                 | acc, Continue -> aux acc xs
                 | acc, Not_deeper limit ->
                 (match up limit nxt with
                        | [] -> acc, None
                        | (coord, t) :: nxt -> acc, Some (coord, nxt, t))
                 | acc,Stop -> acc,None in
         (match aux acc l with
         | acc, None -> acc
         | acc, Some (coord, nxt, t) -> loop coord acc nxt t)
      | N (t1, t2, t3, t4) ->
         let lon3 = (lon1 + lon2) / 2 in
         let lat3 = (lat1 + lat2) / 2 in
         let sq1 = (lat1, lat3, lon1, lon3)
         and sq2 = (lat3, lat2, lon1, lon3)
         and sq3 = (lat1, lat3, lon3, lon2)
         and sq4 = (lat3, lat2, lon3, lon2) in
         match inter sq1, inter sq2, inter sq3, inter sq4 with
         | false, f2, f3, f4 -> loop sq1 acc (List.fold_left (fun acc (b, sq, t) -> if not b then (sq, t) :: acc else acc) nxt [f2, sq2, t2; f3, sq3, t3; f4, sq4, t4]) t1
         | true, false, f3, f4 -> loop sq2 acc (List.fold_left (fun acc (b, sq, t) -> if not b then (sq, t) :: acc else acc) nxt [f3, sq3, t3; f4, sq4, t4]) t2
         | true, true, false, f4 -> loop sq3 acc (List.fold_left (fun acc (b, sq, t) -> if not b then (sq, t) :: acc else acc) nxt [f4, sq4, t4]) t3
         | true, true, true, false -> loop sq4 acc nxt t4
         | true, true, true, true ->
                match nxt with
                 | [] -> acc
                 | (coord, t)::nxt -> loop coord acc nxt t
  in
  let coord = t.bound in
  loop coord acc [] !(t.tree)

let insert tt (y, x) e =
  let cons l = ((y, x), e) :: l in
  let rec loop lat1 lat2 lon1 lon2 t max = match t with
    | C (size,l) when size < tt.slice || tt.depth < max -> C (size + 1, cons l)
    | C (_, l) ->
        let lon3 = (lon1 + lon2) / 2 in
        let lat3 = (lat1 + lat2) / 2 in
        let l1,l2,l3,l4 = List.fold_left (fun (l1, l2, l3, l4) (((y, x), e) as elt) ->
         match x < lon3, y < lat3 with
         | true, true -> elt :: l1, l2, l3, l4
         | true, false -> l1, elt :: l2, l3, l4
         | false, true -> l1, l2, elt :: l3, l4
         | false, false -> l1, l2, l3, elt :: l4) ([],[],[],[]) l in
        let t1, t2, t3, t4 = (C (List.length l1, l1)), (C (List.length l2, l2)), (C (List.length l3, l3)), (C (List.length l4, l4)) in
        begin
         match x < lon3, y < lat3 with
         | true, true -> N (loop lat1 lat3 lon1 lon3 t1 (succ max), t2, t3, t4)
         | true, false -> N (t1, loop lat3 lat2 lon1 lon3 t2 (succ max), t3, t4)
         | false, true -> N (t1, t2,loop lat1 lat3 lon3 lon2 t3 (succ max), t4)
         | false, false -> N (t1, t2, t3, loop lat3 lat2 lon3 lon2 t4 (succ max))
        end
    | N (t1, t2, t3, t4) ->
        let lon3 = (lon1 + lon2) / 2 in
        let lat3 = (lat1 + lat2) / 2 in
        match x < lon3, y < lat3 with
         | true, true -> N (loop lat1 lat3 lon1 lon3 t1 (succ max), t2, t3, t4)
         | true, false -> N (t1, loop lat3 lat2 lon1 lon3 t2 (succ max), t3, t4)
         | false, true -> N (t1, t2, loop lat1 lat3 lon3 lon2 t3 (succ max), t4)
         | false, false -> N (t1, t2, t3, loop lat3 lat2 lon3 lon2 t4 (succ max))
  in
  let lat1, lat2, lon1, lon2 = tt.bound in
  tt.tree := loop lat1 lat2 lon1 lon2 !(tt.tree) 0

let insert_tailrec tt (y,x) e =
  let cons l = ((y,x),e)::l in
  let rec loop lat1 lat2 lon1 lon2 t max k = match t with
    | C (size,l) when size < tt.slice || tt.depth < max -> k (C (size+1,cons l))
    | C (_,l) ->
        let lon3 = (lon1+lon2) / 2 in
        let lat3 = (lat1+lat2) / 2 in
        let l1,l2,l3,l4 = List.fold_left (fun (l1,l2,l3,l4) (((y,x),e) as elt) ->
         match x < lon3, y < lat3 with
         | true,true -> elt::l1,l2,l3,l4
         | true,false -> l1,elt::l2,l3,l4
         | false,true -> l1,l2,elt::l3,l4
         | false,false -> l1,l2,l3,elt::l4) ([],[],[],[]) l in
        let t1,t2,t3,t4 = (C (List.length l1,l1)), (C (List.length l2,l2)), (C (List.length l3,l3)), (C (List.length l4,l4)) in
        begin
         match x<lon3,y<lat3 with
         | true,true -> loop lat1 lat3 lon1 lon3 t1 (succ max) (fun t -> k (N(t,t2,t3,t4)))
         | true,false -> loop lat3 lat2 lon1 lon3 t2 (succ max) (fun t -> k (N(t1,t,t3,t4)))
         | false,true -> loop lat1 lat3 lon3 lon2 t3 (succ max) (fun t -> k (N(t1, t2,t,t4)))
         | false,false -> loop lat3 lat2 lon3 lon2 t4 (succ max) (fun t -> k (N(t1,t2,t3,t)))
        end
    | N(t1,t2,t3,t4) ->
        let lon3 = (lon1+lon2) / 2 in
        let lat3 = (lat1+lat2) / 2 in
        match x<lon3,y<lat3 with
         | true,true -> loop lat1 lat3 lon1 lon3 t1 (succ max) (fun t -> k (N(t,t2,t3,t4)))
         | true,false -> loop lat3 lat2 lon1 lon3 t2 (succ max) (fun t -> k (N(t1,t,t3,t4)))
         | false,true -> loop lat1 lat3 lon3 lon2 t3 (succ max) (fun t -> k (N(t1, t2,t,t4)))
         | false,false -> loop lat3 lat2 lon3 lon2 t4 (succ max) (fun t -> k (N(t1,t2,t3,t)))
  in
  let lat1,lat2,lon1,lon2 = tt.bound in
  loop lat1 lat2 lon1 lon2 !(tt.tree) 0 (fun t -> tt.tree:=t)


let remove tt (y,x) e =
  let rec loop lat1 lat2 lon1 lon2 t = match t with
    | C (size, l) -> let rem, l = List.partition ((fun (_, e') -> e' = e)) l in
      C (size - (List.length rem), l)
    | N(t1, t2, t3, t4) ->
        let lon3 = lon1 + lon2 / 2 in
        let lat3 = lat1 + lat2 / 2 in
        match x < lon3, y <lat3 with
         | true, true -> N(loop lat1 lat3 lon1 lon3 t1,t2,t3,t4)
         | true,false -> N(t1, loop lat3 lat2 lon1 lon3 t2,t3,t4)
         | false,true -> N(t1, t2, loop lat1 lat3 lon3 lon2 t3,t4)
         | false,false ->N(t1, t2, t3, loop lat3 lat2 lon3 lon2 t4)
  in
  let lat1,lat2,lon1,lon2 = tt.bound in
  tt.tree := loop lat1 lat2 lon1 lon2 !(tt.tree)


let lmax = function
  | [] -> assert false
  | x::xs -> List.fold_left max x xs

let depth tt =
  let rec loop t = match t with
    | C (_,_) -> 1
    | N(t1,t2,t3,t4) -> 1+lmax (List.map loop [t1;t2;t3;t4])
  in loop !(tt.tree)

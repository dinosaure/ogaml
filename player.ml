module SV = Sdlvideo
module SK = Sdlkey
module SL = Sdlloader

type velocity =
  | TT
  | T
  | N
  | B
  | BB

let image = SL.load_image "data/r-typesheet42.gif"

let draw src_rect screen dst_rect =
  let dst_rect = SV.rect dst_rect#get_x dst_rect#get_y dst_rect#get_w dst_rect#get_h in
    SV.blit_surface ~src_rect ~src:image ~dst:screen ~dst_rect ()

let draw_1 = (SV.rect 0 0 33 18)
let draw_2 = (SV.rect 33 0 33 18)
let draw_3 = (SV.rect 66 0 33 18)
let draw_4 = (SV.rect 99 0 33 18)
let draw_5 = (SV.rect 132 0 33 18)

let private_draw velocity =
  let src_rect = match velocity with
    | TT -> draw_1
    | T -> draw_2
    | N -> draw_3
    | B -> draw_4
    | BB -> draw_5
  in draw src_rect

let vertical event (x, y) old_velocity =
  let t = try Event.find SK.KEY_UP event with _ -> 0 in
  let d = try Event.find SK.KEY_DOWN event with _ -> 0 in
  let velocity = t - d in
  match velocity < -5, velocity < 0, velocity = 0, velocity > 0, velocity > 5  with
    | true, _, _, _, _ when old_velocity = T || old_velocity = TT
      -> TT, (x, y + 1)
    | _, _, _, _, true when old_velocity = B || old_velocity = BB
      -> BB, (x, y - 1)
    | _, true, _, _, _ -> T, (x, y + 1)
    | _, _, _, true, _ -> B, (x, y - 1)
    | _ when old_velocity = TT -> T, (x, y)
    | _ when old_velocity = BB -> B, (x, y)
    | _ -> N, (x, y)

let has_collision = function
  | Rtype.Player -> false
  | Rtype.WhiteWall -> true
  | Rtype.Missile -> true

let private_update event self old_velocity old_shooting old_rectangle quadtree =
  let prediction rectangle =
    Qt.find quadtree (Qt.from_rectangle rectangle)
      (fun acc _ obj ->
        if has_collision obj#get_type
          && Rectangle.inter obj#to_rect rectangle
          && obj <> self
        then (true, Qt.Stop)
        else (false, Qt.Continue)) false in
  let horizontal (x, y) = match
    (Event.exists (fun k _ -> k = SK.KEY_LEFT) event),
    (Event.exists (fun k _ -> k = SK.KEY_RIGHT) event)
  with
    | true, false -> (x - 1, y)
    | false, true -> (x + 1, y)
    | _ -> (x, y)
  in let (new_velocity, new_position) = vertical event (horizontal (0, 0)) old_velocity
  in let new_shooting =
    match old_shooting, Event.exists (fun k _ -> k = SK.KEY_SPACE) event with
    | None, true -> (1, false)
    | None, false -> (0, false)
    | Some a, true -> (a + 1, false)
    | Some a, false -> (a, true)
  in if prediction (old_rectangle#move new_position)
    then old_velocity, (0, 0), new_shooting
    else new_velocity, new_position, new_shooting

class t x y =
  object (s : 'a)
    inherit Rtype.t Rtype.Player
    inherit Rectangle.t x y 33 18
    inherit Update.t
    inherit Draw.t

    val velocity = N
    method private get_velocity = velocity

    val shooting : int option = None
    method private get_shooting = shooting

    method update : 'a Update.f =
      (fun event quadtree ->
        let (new_velocity, (x', y'), (intensity, shooting')) =
          private_update event s s#get_velocity s#get_shooting s#to_rect quadtree
        in {< velocity = new_velocity; x = x + x'; y = y + y'; shooting = if shooting' then None else Some intensity >})

    method draw screen =
      private_draw s#get_velocity screen s#to_rect
  end

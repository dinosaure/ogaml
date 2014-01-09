module SL = Sdlloader
module SV = Sdlvideo

type frame =
  | Frame_1
  | Frame_2

let image = SL.load_image "data/r-typesheet1.gif"

let missile_i1_d1_f1 = SV.rect 232 103 16 12
let missile_i1_d1_f2 = SV.rect 249 103 16 12
let missile_i2_d1_f1 = SV.rect 200 120 32 12
let missile_i2_d1_f2 = SV.rect 233 120 32 12
let missile_i3_d1_f1 = SV.rect 168 136 48 14
let missile_i3_d1_f2 = SV.rect 217 136 48 14
let missile_i4_d1_f1 = SV.rect 136 154 64 14
let missile_i4_d1_f2 = SV.rect 201 154 64 14
let missile_i5_d1_f1 = SV.rect 104 170 80 16
let missile_i5_d1_f2 = SV.rect 185 170 80 16

let draw src_rect screen dst_rect =
  let dst_rect = SV.rect dst_rect#get_x dst_rect#get_y dst_rect#get_w dst_rect#get_h in
    SV.blit_surface ~src_rect ~src:image ~dst:screen ~dst_rect ()

let get_dimension = function
  | 0 -> (16, 12)
  | 1 -> (32, 12)
  | 2 -> (48, 14)
  | 3 -> (64, 14)
  | _ -> (80, 16)

let max_time = 5

let private_update event self frame (i, m) vector time quadtree =
  let upgrade i = if i < 4 then i + 1 else i in
  if time = 0 && i < m
  then match frame with
    | Frame_1 -> (Frame_2, i, vector, get_dimension i, max_time)
    | Frame_2 -> (Frame_1, upgrade i, vector, get_dimension (upgrade i), max_time)
  else match frame with
    | Frame_1 -> (Frame_2, i, vector, get_dimension i, time - 1)
    | Frame_2 -> (Frame_1, i, vector, get_dimension i, time - 1)

let private_draw frame intensity =
  let src_rect = match intensity, frame with
    | 0, Frame_1 -> missile_i1_d1_f1
    | 0, Frame_2 -> missile_i1_d1_f2
    | 1, Frame_1 -> missile_i2_d1_f1
    | 1, Frame_2 -> missile_i2_d1_f2
    | 2, Frame_1 -> missile_i3_d1_f1
    | 2, Frame_2 -> missile_i3_d1_f2
    | 3, Frame_1 -> missile_i4_d1_f1
    | 3, Frame_2 -> missile_i4_d1_f2
    | _, Frame_1 -> missile_i5_d1_f1
    | _, Frame_2 -> missile_i5_d1_f2
  in draw src_rect

class t x y vector' intensity' =
  object (s : 'a)
    inherit Rtype.t Rtype.Missile
    inherit Rectangle.t x y 16 12
    inherit Update.t
    inherit Draw.t

    val intensity = 0
    method private get_intensity = intensity
    val max_intensity = if intensity' < 5 then intensity' else 4
    method private get_max_intensity = max_intensity
    val vector = vector'
    method private get_vector = vector
    val frame = Frame_1
    method private get_frame = frame
    val time = max_time
    method private get_time = time

    method update : 'a Update.f =
      (fun event quadtree ->
        let (new_frame, new_intensity, (x', y'), (w', h'), time') =
          private_update event s s#get_frame (s#get_intensity, s#get_max_intensity) s#get_vector s#get_time quadtree
        in {< frame = new_frame; intensity = new_intensity; x = x + x'; y = y + y'; w = w'; h = h'; time = time' >})

    method draw screen =
      private_draw s#get_frame s#get_intensity screen s#to_rect
  end

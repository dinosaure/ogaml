module SE = Sdlevent
module SK = Sdlkey
module SV = Sdlvideo

type 'a t = {
  event: int Event.t;
  screen: SV.surface;
  quadtree: 'a Qt.t;
}

let new_world width height = {
  event = Event.empty;
  screen = SV.set_video_mode width height [`DOUBLEBUF];
  quadtree = Qt.create ~bound:(Qt.rect 0 width 0 height) ~depth:5 1;
}

let add_object o ({ event; screen; quadtree }) =
  let rectangle = o#to_rect in
  Qt.insert quadtree (Qt.from_rectangle rectangle) o;
  { event; screen; quadtree }

let update { event; screen; quadtree } =
  let new_event = Event.update event in
  let new_screen = screen in
  let new_quadtree =
    Qt.fold
      (fun acc (_, e) -> let e = e#update new_event quadtree in
        Qt.clean_insert acc (Qt.from_rectangle e#to_rect) e)
      (Qt.copy quadtree) quadtree
  in
  { event = new_event;
    screen = new_screen;
    quadtree = new_quadtree }

let draw { screen; quadtree; _ } =
  List.iter (fun (_, x) -> (x#to_draw)#draw screen) (Qt.to_list quadtree);
  SV.flip screen

let get_screen { screen; _ } = screen
let get_quadtree { quadtree; _ } = quadtree

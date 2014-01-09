module SE = Sdlevent
module SM = Sdlmixer
module SV = Sdlvideo
module SK = Sdlkey
module ST = Sdlttf
module SD = Sdl

let time_of_handle_event = 1.0 /. 60.0

let display_background screen =
  SV.fill_rect screen (SV.map_RGB screen (0xFF, 0x6B, 0x6B))

let rec wait time = match (Sys.time () -. time) < time_of_handle_event with
  | true -> wait time
  | false -> ()

let rock = [
  new Player.t 0 0;
  new WhiteWall.t 50 50 10 10;
  new Missile.t 10 100 (5, 0) 4;
]

let run () =
  let world = World.new_world 256 240 in
  let rec aux world =
    let time = Sys.time () in
    display_background (World.get_screen world);
    Qt.draw (World.get_screen world) (World.get_quadtree world);
    World.draw world;
    wait time;
    aux (World.update world)
  in
    aux (List.fold_left (fun a e -> World.add_object e a) world rock)

let main () =
  SD.init [`VIDEO; `AUDIO];
  at_exit SD.quit;
  ST.init ();
  at_exit ST.quit;
  SM.open_audio ();
  at_exit SM.close_audio;

  run ()


let () = main ()

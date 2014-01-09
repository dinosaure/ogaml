module SK = Sdlkey
module SE = Sdlevent

module OrderedKey = struct
  type t = SK.t
  let compare a b = (SK.int_of_key a) - (SK.int_of_key b)
end

include Map.Make(OrderedKey)

let private_add key velocity =
  try let _ = find key velocity in velocity
  with _ -> add key 1 velocity

let update ?(at_escape = fun () -> exit 0) velocity =
  let rec aux velocity = BatOption.map_default (function
    | SE.KEYDOWN { SE.keysym = SK.KEY_ESCAPE; _ } -> at_escape (); aux velocity
    | SE.KEYDOWN { SE.keysym = a; _ } -> aux (private_add a velocity)
    | SE.KEYUP { SE.keysym = a; _ } -> aux (remove a velocity)
    | _ -> aux velocity) velocity (SE.poll ())
  in aux (map (fun a -> a + 1) velocity)

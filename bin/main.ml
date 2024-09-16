open Claudius

type point = { x : float; y : float; z : float }

let color_mung cols =
  List.map
    (fun (r, g, b) ->
      (int_of_float (255. *. r) lsl 16)
      lor (int_of_float (255. *. g) lsl 8)
      lor (int_of_float (255. *. b) lsl 0))
    cols

let model coordlist points =
  List.mapi
    (fun i (a, b, c) -> ((points.(a), points.(b), points.(c)), i + 1))
    coordlist

let rotate_x (a : float) (p : point) : point =
  {
    p with
    y = (p.y *. cos a) -. (p.z *. sin a);
    z = (p.y *. sin a) +. (p.z *. cos a);
  }

let rotate_y (a : float) (p : point) : point =
  {
    p with
    x = (p.x *. cos a) -. (p.z *. sin a);
    z = (p.x *. sin a) +. (p.z *. cos a);
  }

let rotate_z (a : float) (p : point) : point =
  {
    p with
    x = (p.x *. cos a) -. (p.y *. sin a);
    y = (p.x *. sin a) +. (p.y *. cos a);
  }

let midpoint t =
  let p1, p2, p3 = t in
  {
    x = (p1.x +. p2.x +. p3.x) /. 3.;
    y = (p1.y +. p2.y +. p3.y) /. 3.;
    z = (p1.z +. p2.z +. p3.z) /. 3.;
  }

let cp (p1 : Primitives.point) (p2 : Primitives.point) =
  (p2.x - p1.x) * (p2.y + p1.y)

let filter_tri t =
  match t with
  | Primitives.Triangle (p1, p2, p3, _) -> cp p1 p2 + cp p2 p3 + cp p3 p1 < 0
  | Primitives.FilledTriangle (p1, p2, p3, _) ->
      cp p1 p2 + cp p2 p3 + cp p3 p1 < 0
  | _ -> true

let furthest_corner t =
  let p1, p2, p3 = t in
  let pt = if p1.z < p2.z then p1 else p2 in
  if pt.z < p3.z then pt.z else p3.z

let point_z_cmp v1 v2 : int =
  let t1, _ = v1 and t2, _ = v2 in
  let a = furthest_corner t1 and b = furthest_corner t2 in
  if a == b then
    let a = midpoint t1 and b = midpoint t2 in
    if a.z == b.z then 0 else if a.z > b.z then 1 else -1
  else if a > b then 1
  else -1

let proj ft s e : Primitives.point =
  let width, height = Screen.dimensions s in
  let m = 1000. +. (cos (ft /. 30.) *. 200.) in
  {
    x = (width / 2) + int_of_float (m *. e.x /. (e.z +. 400.));
    y = (height / 2) + int_of_float (m *. e.y /. (e.z +. 400.));
  }

let render_to_primitives ft s points =
  List.map
    (fun t ->
      let (a, b, c), i = t in
      let p2 =
        Primitives.FilledTriangle (proj ft s a, proj ft s b, proj ft s c, i)
      in
      p2)
    points

let p = ref 0.
let prev = ref Base.KeyCodeSet.empty

let tick ship _t s _p i =
  (* input *)
  if Base.KeyCodeSet.is_empty !prev && Base.KeyCodeSet.is_empty i then
    p := !p +. 1.;
  let ft = !p in
  prev := i;

  (* cls *)
  let fb = Framebuffer.init (Screen.dimensions s) (fun _ _ -> 0) in

  let coords, points = ship in

  Array.map (fun (x, y, z) -> { x; y; z }) points
  |> Array.map (fun p ->
         rotate_y (0.02 *. ft) p
         |> rotate_x (0.01 *. ft)
         |> rotate_z (0.005 *. ft))
  |> model coords |> List.sort point_z_cmp |> render_to_primitives ft s
  |> List.filter filter_tri |> Framebuffer.render fb;

  fb

let () =
  let colors, coords, points = Krait.model in

  let p = Palette.of_list (0x00 :: color_mung colors) in
  Screen.create 640 480 1 p |> Base.run "Carnage" None (tick (coords, points))

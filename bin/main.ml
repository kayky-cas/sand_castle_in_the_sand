module State = struct
  type t = {
    mutable board : int32 array array;
    width : int;
    height : int;
    mutable sand_count : int;
  }

  let create ~width ~height =
    let board = Array.make_matrix height width 0l in
    { board; width; height; sand_count = 0 }

  let update t =
    let width = t.width in
    let height = t.height in
    let board = t.board in

    let new_board = Array.make_matrix height width 0l in

    for i = 0 to height - 1 do
      for j = 0 to width - 1 do
        let cell = board.(i).(j) in
        if cell > 0l then (
          let south = i + 1 in
          let west = j - 1 in
          let east = j + 1 in

          let side = if Random.bool () then ref west else ref east in

          if !side < 0 then side := east
          else if !side >= width then side := west;

          if i < t.height - 1 && board.(south).(j) = 0l then (
            new_board.(i).(j) <- 0l;
            new_board.(south).(j) <- cell)
          else if i < t.height - 2 then
            if board.(south + 1).(!side) = 0l then (
              new_board.(i).(j) <- 0l;
              new_board.(south + 1).(!side) <- cell)
            else new_board.(i).(j) <- cell
          else new_board.(i).(j) <- cell)
      done
    done;

    t.board <- new_board

  let render t =
    let window_width = Raylib.get_screen_width () in
    let window_height = Raylib.get_screen_height () in

    let width = window_width / t.width in
    let height = window_height / t.height in

    let board = t.board in

    Raylib.draw_rectangle 0 0 (width * t.width) (height * t.height)
      Raylib.Color.blue;

    for i = 0 to t.height - 1 do
      for j = 0 to t.width - 1 do
        let cell = board.(i).(j) in

        let x = j * width in
        let y = i * height in
        if cell = 1l then
          Raylib.draw_rectangle x y width height Raylib.Color.yellow
        else if cell = 2l then
          Raylib.draw_rectangle x y width height Raylib.Color.orange
        else if cell = 3l then
          Raylib.draw_rectangle x y width height Raylib.Color.red
      done
    done

  let handle_mouse t =
    if Raylib.is_mouse_button_down Raylib.MouseButton.Left then
      let window_width = Raylib.get_screen_width () in
      let window_height = Raylib.get_screen_height () in
      let width = window_width / t.width in
      let height = window_height / t.height in
      let x = Raylib.get_mouse_x () / width in
      let y = Raylib.get_mouse_y () / height in

      let tird = t.width * (t.height / 3) in

      for i = -3 to 3 do
        for j = -3 to 3 do
          let x = x + i in
          let y = y + j in
          if
            x >= 0 && x < t.width && y >= 0 && y < t.height
            && t.board.(y).(x) = 0l
          then (
            t.sand_count <- t.sand_count + 1;
            t.board.(y).(x) <-
              (if t.sand_count < tird then 1l
               else if t.sand_count < tird * 2 then 2l
               else 3l))
        done
      done
end

let setup () =
  Raylib.set_config_flags [ Raylib.ConfigFlags.Window_resizable ];
  Raylib.init_window 800 800 "Sand castle in the Sand";
  Raylib.set_target_fps 60

let rec loop s =
  if Raylib.window_should_close () then Raylib.close_window ()
  else Raylib.begin_drawing ();
  State.render s;
  State.handle_mouse s;
  State.update s;
  Raylib.end_drawing ();
  loop s

let () =
  setup ();
  let _ = loop (State.create ~width:120 ~height:120) in
  Raylib.close_window ()

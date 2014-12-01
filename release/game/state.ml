module GameState = struct
  type state = int

  type red_name = string option ref
  type blue_name = string option ref

  let initial_state () = 42

  let get_red_name s = None
  let get_blue_name s = None
end

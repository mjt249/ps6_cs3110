module State = struct
  type state = int

  type red_name = ref string option
  type blue_name = ref string option

  let initial_state () = 42
end

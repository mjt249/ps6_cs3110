module GameState : sig 
  type state

  val initial_state: unit -> state

  val get_red_name: state -> string option
  val get_blue_name: state -> string option
end

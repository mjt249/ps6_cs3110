module type STATE = sig 
  type state

  val initial_state: unit -> state
end

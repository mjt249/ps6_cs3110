open Definitions

module GameState : sig 
  type state

  (* Create a blank state with all needed state variables 
   * initialized to begin the draft phase of the game *)
  val initial_state: unit -> state

  val get_red_name: state -> string option
  val get_blue_name: state -> string option
  val get_move_list: state -> move list
  val get_steammon_list: state -> steammon list
  val set_red_name: state -> string -> unit
  val set_blue_name: state -> string -> unit
  val set_move_list: state -> move list -> unit
  val set_steammon_list: state -> steammon list -> unit
end

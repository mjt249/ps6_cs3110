open Definitions

module GameState : sig 
  type state

  (* Create a blank state with all needed state variables 
   * initialized to begin the draft phase of the game *)
  val initial_state: unit -> state

  val get_name: state -> color -> string option
  val get_move_list: state -> move list
  val get_steammon_list: state -> steammon list
  val get_inv: state -> color -> inventory
  val get_exp: state -> color -> action

  val set_name: state -> color -> string -> unit
  val set_move_list: state -> move list -> unit
  val set_steammon_list: state -> steammon list -> unit
  val set_inv: state -> color -> inventory -> unit
  val set_exp: state -> color -> action -> unit
end

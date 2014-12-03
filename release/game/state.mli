open Definitions
open Util

module GameState : sig 
  type state
  type phase = TeamName | Draft | Inventory | Battle

  (* Create a blank state with all needed state variables 
   * initialized to begin the draft phase of the game *)
  val initial_state: unit -> state

  val get_name: state -> color -> string option
  val get_move_list: state -> move list
  val get_base_mons: state -> steammon list

  (* Returns the list of steammon of the player with the 
   * active steammon at the head of the list if such steammon exists. *)
  val get_steammon_list: state -> color -> steammon list

  val get_reserve_pool: state -> color -> steammon Table.t

  val get_inv: state -> color -> inventory
  val get_exp: state -> color -> action
  val get_creds: state -> color -> int
  val get_phase: state -> phase
  val get_active_mon: state -> color -> steammon option

  val set_name: state -> color -> string -> unit
  val set_inv: state -> color -> inventory -> unit
  val set_exp: state -> color -> action -> unit
  val set_creds: state -> color -> int -> unit
  val set_phase: state -> phase -> unit
  val set_active_mon: state -> color -> steammon -> unit

  val add_reserve_steammon: state -> color -> steammon -> unit
  val remove_reserve_steammon: state -> color -> steammon -> unit

  (* If there is an active steammon, it is swapped with the given
   * steammon and the swapped one is added to the reserve and the
   * given steammon is removed from the reserve pool.  If there is 
   * no active steammon, the given steammon is removed from reserve. *)
  val swap_active_steammon: state -> color -> steammon -> unit
end

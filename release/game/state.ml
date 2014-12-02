
open Constants
open Definitions


module GameState = struct
  (* State is represented as a record with mutable fields
   * to store mutating game state. The 'game' type used by 
   * server and game should be set to GameState.state and
   * all internal mutating of the fields when game actions
   * happen should get updated using setters. All reads should 
   * be performed via getters. *)
  type player = {
    mutable inv : inventory;
    mutable active_steammon : steammon option;
  }

  type state = { 
    mutable red_name : string option;
    mutable blue_name : string option;
    mutable mvs : move list;
    mutable mons : steammon list;
    mutable red : player;
    mutable blue : player;
  }

  let init_player () = {inv = []; active_steammon = None;}
  let initial_state () = {
    red_name = None;
    blue_name = None;
    mvs = [];
    mons = [];
    red = init_player ();
    blue = init_player ();


    (*updates Steammon_Credits. credits may be negative*)
let update_steammon_credits s team credits =
	(*add credits to Steammon_Credits*) None

  }

  let get_red_name s = s.red_name
  let get_blue_name s = s.blue_name
  let get_move_list s = s.mvs
  let get_steammon_list s = s.mons
  let get_red_inv s = s.red.inv
  let get_blue_inv s = s.blue.inv

  let set_red_name s name = s.red_name <- (Some name)
  let set_blue_name s name = s.blue_name <- (Some name)
  let set_move_list s mv_list = s.mvs <- mv_list
  let set_steammon_list s mon_list = s.mons <- mon_list
  let set_red_inv s inv = s.red.inv <- inv
  let set_blue_inv s inv = s.blue.inv <- inv
end
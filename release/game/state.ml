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
    mutable expected_action : action;
  }

  type state = { 
    mutable red_name : string option;
    mutable blue_name : string option;
    mutable mvs : move list;
    mutable mons : steammon list;
    mutable red : player;
    mutable blue : player;
  }

  let init_red () = {
    inv = []; 
    active_steammon = None;
    expected_action = SendTeamName "Red";
  }

  let init_blue () = {
    inv = []; 
    active_steammon = None;
    expected_action = SendTeamName "Blue";
  }

  let initial_state () = {
    red_name = None;
    blue_name = None;
    mvs = [];
    mons = [];
    red = init_red ();
    blue = init_blue ();
  }

  let get_red_name s = s.red_name
  let get_blue_name s = s.blue_name
  let get_move_list s = s.mvs
  let get_steammon_list s = s.mons
  let get_red_inv s = s.red.inv
  let get_blue_inv s = s.blue.inv
  let get_red_exp s = s.red.expected_action
  let get_blue_exp s = s.blue.expected_action

  let set_red_name s name = s.red_name <- (Some name)
  let set_blue_name s name = s.blue_name <- (Some name)
  let set_move_list s mv_list = s.mvs <- mv_list
  let set_steammon_list s mon_list = s.mons <- mon_list
  let set_red_inv s inv = s.red.inv <- inv
  let set_blue_inv s inv = s.blue.inv <- inv
  let set_red_exp s a = s.red.expected_action <- a
  let set_blue_exp s a = s.blue.expected_action <- a
end

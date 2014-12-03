
open Constants
open Definitions
open Constants


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
    mutable turn: color;

  }

  let init_red () = {
    inv = []; 
    active_steammon = None;
    expected_action = SendTeamName "Red";
    steammons = [];
    credits = cSTEAMMON_CREDITS; 
  }

  let init_blue () = {
    inv = []; 
    active_steammon = None;
    expected_action = SendTeamName "Blue";
    steammons = [];
    credits = cSTEAMMON_CREDITS; 
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

    first = Red;
  }

  let get_name s c = 
    match c with 
    | Red -> s.red_name 
    | Blue -> s.blue_name
  let get_move_list s = s.mvs
  let get_steammon_list s = s.mons
  let get_player_steammons s c = 
    match c with
    | Red -> s.red.steammons
    | Blue -> s.blue.steammons
  let get_inv s c = 
    match c with
    | Red -> s.red.inv
    | Blue -> s.blue.inv
  let get_exp s c = 
    match c with
    | Red -> s.red.expected_action
    | Blue -> s.blue.expected_action
  let get_creds s c = 
    match c with
    | Red -> s.red.credits
    | Blue -> s.blue.credits

  let get_turn s : color = s.turn



  let set_name s c name = 
    match c with
    | Red -> s.red_name <- (Some name)
    | Blue -> s.blue_name <- (Some name)
  let set_move_list s mv_list = s.mvs <- mv_list
  let set_steammon_list s mon_list = s.mons <- mon_list

  let set_player_steammons s c l = 
    match c with
    | Red -> s.red.steammons <- l
    | Blue -> s.blue.steammons <- l
  let set_inv s c inv = 
    match c with
    | Red -> s.red.inv <- inv
    | Blue -> s.blue.inv <- inv
  let set_exp s c a = 
    match c with
    | Red -> s.red.expected_action <- a
    | Blue -> s.blue.expected_action <- a
  let set_creds s c m = 
    match c with
    | Red -> s.red.credits <- m
    | Blue -> s.blue.credits <- m

  let set_turn s c = 
      s.turn = c


  (* Comparing the constructors for the actions to determine 
   * whether the expected action matches the responded action *)
  let compare_expected_action (s:state) (c:color) (ac:action) : bool =
    let exp_action = match c with
    | Red -> s.red.expected_action
    | Blue -> s.blue.expected_action in

    match (ac, exp_action) with
    | (SendTeamName _, SendTeamName _) -> true      
    | (SelectStarter _, SelectStarter _) -> true
    | (PickSteammon _, PickSteammon _) -> true
    | (PickInventory _, PickInventory _) -> true
    | (SwitchSteammon _, SwitchSteammon _) -> true
    | (UseItem _, UseItem _) -> true
    | (UseMove _, UseMove _) -> true
    | _ -> false
end


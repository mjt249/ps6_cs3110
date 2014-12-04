
open Constants
open Definitions
open Constants
open Util


module GameState = struct
  (* State is represented as a record with mutable fields
   * to store mutating game state. The 'game' type used by 
   * server and game should be set to GameState.state and
   * all internal mutating of the fields when game actions
   * happen should get updated using setters. All reads should 
   * be performed via getters. *)
  type phase = TeamName | Draft | Inventory | Starter | Battle
  exception NO_ACTIVE_STEAMMON

  type active_steammon = {
    mutable mon : steammon;
    mutable can_use_moves : bool;
    mutable will_attack_self : bool;
  }

  type player = {
    mutable inv : inventory;

    mutable active_mon : active_steammon option;
    mutable steammons : steammon Table.t option;
    mutable credits : int;

  }

  type state = { 
    mutable red_name : string option;
    mutable blue_name : string option;
    mutable mvs : move Table.t;
    mutable base_mons : steammon Table.t;
    mutable red : player;
    mutable blue : player;
    
    mutable draft_mons : steammon Table.t;
    mutable turn: color;


    mutable phase : phase;

  }

  let init_red () = {
    inv = []; 
    active_mon = None;
    steammons = None;
    credits = cSTEAMMON_CREDITS; 
  }

  let init_blue () = {
    inv = []; 
    active_mon = None;
    steammons = None;
    credits = cSTEAMMON_CREDITS; 
  }


  let initial_state () =
    Initialization.init_pool "moves.csv" "steammon.csv";
    {

    red_name = None;
    blue_name = None;
    mvs = Initialization.move_table;
    base_mons = Initialization.mon_table;
    red = init_red ();
    blue = init_blue ();
    draft_mons = Initialization.mon_table;
    turn = Red;
    phase = TeamName;
    }

  let get_name s c = 
    match c with 
    | Red -> s.red_name 
    | Blue -> s.blue_name
  let get_move_list s = hash_to_list s.mvs
  let get_moves s = s.mvs
  let get_base_mons s = hash_to_list s.base_mons

  (* Given a state and a player colour, returns all the
   * steammons the player possesses *)
  let get_steammon_list s c : steammon list = 
    let player = match c with
    | Red -> s.red
    | Blue -> s.blue in
    match (player.steammons, player.active_mon)  with
    | (None, None) -> [] 
    | (None, Some m) -> [m.mon]
    | (Some ms, None) -> hash_to_list ms
    | (Some ms, Some m) -> (m.mon)::(hash_to_list ms)

  let get_reserve_pool s c : steammon Table.t =
    let player = match c with
    | Red -> s.red
    | Blue -> s.blue in
    match player.steammons with
    | Some tbl -> tbl
    | None -> failwith "Reserve pool not initialized"

  let get_draft_mons s : steammon Table.t = s.draft_mons
  
  let get_draft_finished s : bool = 
      let r = s.red.steammons in
      let b = s.blue.steammons in
      match (r,b) with
      | (Some tbl_r, Some tbl_b) ->
       let r_nbr = Table.length tbl_r in
      let b_nbr = Table.length tbl_b in
      ((r_nbr + b_nbr) = (cNUM_PICKS * 2))
      | _ -> failwith "steammons tbl should have been initiated"

  (* ************************* *)
  (* might not need 
  let is_active_red (s: state) (steam: steammon) = 
    match s.red.active_steammon with
    | None -> false
    | Some mon -> steam.species = mon.species
  let is_active_blue (s: state) (steam: steammon) = 
    match s.blue.active_steammon with
    | None -> false
    | Some mon -> steam.species = mon.species
   *)

  let get_inv s c = 
    match c with
    | Red -> s.red.inv
    | Blue -> s.blue.inv
  let get_creds s c = 
    match c with
    | Red -> s.red.credits
    | Blue -> s.blue.credits


  let get_turn s : color = s.turn


  let get_phase s = 
    s.phase
  (* eff speed of active steammon *)
  let get_eff_speed s c = 
    match c with
    | Red -> (match s.red.active_mon with
			 | None -> 0
			 | Some x -> x.mon.speed)
    | Blue -> match s.blue.active_mon with
			 | None -> 0
			 | Some x -> x.mon.speed
  let get_active_mon s c =
    let player = match c with
    | Red -> s.red
    | Blue -> s.blue in
    match player.active_mon with
    | None -> None
    | Some active_mon -> Some (active_mon.mon)
  (* eff speed of active steammon *)
  let get_curr_hp s c = 
    match c with
    | Red -> (match s.red.active_mon with
			 | None -> 0
			 | Some x -> x.mon.curr_hp)
    | Blue -> match s.blue.active_mon with
			 | None -> 0
			 | Some x -> x.mon.curr_hp
  let get_max_hp s c = 
    match c with
    | Red -> (match s.red.active_mon with
			 | None -> 0
			 | Some x -> x.mon.max_hp)
    | Blue -> match s.blue.active_mon with
			 | None -> 0
			 | Some x -> x.mon.max_hp
  let get_can_use_moves s c = 
    match c with
    | Red -> (match s.red.active_mon with
			 | None -> raise NO_ACTIVE_STEAMMON
			 | Some x -> x.can_use_moves)
    | Blue -> match s.blue.active_mon with
			 | None -> raise NO_ACTIVE_STEAMMON
			 | Some x -> x.can_use_moves

  let set_name s c name = 
    match c with
    | Red -> s.red_name <- (Some name)
    | Blue -> s.blue_name <- (Some name)

  let set_move_list s mv = s.mvs <- mv
  
  let set_draft_mons s d = s.draft_mons <- d


  let set_inv s c inv = 
    match c with
    | Red -> s.red.inv <- inv
    | Blue -> s.blue.inv <- inv
  let set_creds s c m = 
    match c with
    | Red -> s.red.credits <- m
    | Blue -> s.blue.credits <- m
  let set_phase s p =
    s.phase <- p
  let set_can_use_moves s c boolean = 
    match c with
    | Red -> (match s.red.active_mon with
	     | None -> raise NO_ACTIVE_STEAMMON
	     | Some x -> x.can_use_moves <- boolean)
    | Blue -> match s.blue.active_mon with
	     | None -> raise NO_ACTIVE_STEAMMON
	     | Some x -> x.can_use_moves <- boolean
  let set_will_attack_self s c boolean = 
    match c with
    | Red -> (match s.red.active_mon with
	     | None -> raise NO_ACTIVE_STEAMMON
	     | Some x -> x.will_attack_self <- boolean)
    | Blue -> match s.blue.active_mon with
	     | None -> raise NO_ACTIVE_STEAMMON
	     | Some x -> x.will_attack_self <- boolean
  let set_eff_speed s c mon eff_speed: unit = 
    let new_mon = { species = mon.species; 
		    curr_hp = mon.curr_hp; 
		    max_hp = mon.max_hp;
		    first_type = mon.first_type;
		    second_type = mon.second_type;
		    first_move = mon.first_move;
		    second_move = mon.second_move;
		    third_move = mon.third_move;
		    fourth_move = mon.fourth_move;
		    attack = mon.attack;
		    spl_attack = mon.spl_attack;
		    defense = mon.defense;
		    spl_defense = mon.spl_defense;
		    speed = eff_speed;
		    status = mon.status;
		    mods = mon.mods;
		    cost = mon.cost } in
    match c with
    | Red -> (match s.red.active_mon with
	     | None -> raise NO_ACTIVE_STEAMMON 
	     | Some x -> x.mon <- new_mon)
    | Blue -> match s.blue.active_mon with
	      | None -> raise NO_ACTIVE_STEAMMON
	      | Some x -> x.mon <- new_mon
  (* applies to currently active steammon *)
  let set_hp s c mon value : unit = 
    let new_mon = { species = mon.species; 
		    curr_hp = value; 
		    max_hp = mon.max_hp;
		    first_type = mon.first_type;
		    second_type = mon.second_type;
		    first_move = mon.first_move;
		    second_move = mon.second_move;
		    third_move = mon.third_move;
		    fourth_move = mon.fourth_move;
		    attack = mon.attack;
		    spl_attack = mon.spl_attack;
		    defense = mon.defense;
		    spl_defense = mon.spl_defense;
		    speed = mon.speed;
		    status = mon.status;
		    mods = mon.mods;
		    cost = mon.cost } in
    match c with
    | Red -> (match s.red.active_mon with
	     | None -> raise NO_ACTIVE_STEAMMON 
	     | Some x -> x.mon <- new_mon)
    | Blue -> match s.blue.active_mon with
	      | None -> raise NO_ACTIVE_STEAMMON
	      | Some x -> x.mon <- new_mon
  let set_status s c mon stat: unit = 
    let new_mon = { species = mon.species; 
		    curr_hp = mon.curr_hp; 
		    max_hp = mon.max_hp;
		    first_type = mon.first_type;
		    second_type = mon.second_type;
		    first_move = mon.first_move;
		    second_move = mon.second_move;
		    third_move = mon.third_move;
		    fourth_move = mon.fourth_move;
		    attack = mon.attack;
		    spl_attack = mon.spl_attack;
		    defense = mon.defense;
		    spl_defense = mon.spl_defense;
		    speed = mon.speed;
		    status = stat;
		    mods = mon.mods;
		    cost = mon.cost } in
    match c with
    | Red -> (match s.red.active_mon with
	     | None -> raise NO_ACTIVE_STEAMMON 
	     | Some x -> x.mon <- new_mon)
    | Blue -> match s.blue.active_mon with
	      | None -> raise NO_ACTIVE_STEAMMON
	      | Some x -> x.mon <- new_mon
  let set_active_mon s c mon =
    (* Update ALL FIELDS OF THE ACTIVE STEAMMON *)
    let player = match c with
    | Red -> s.red
    | Blue -> s.blue in
    match mon with 
    | None -> player.active_mon <- None
    | Some m -> 
        match player.active_mon with
        | Some active_mon -> active_mon.mon <- m
        | None -> player.active_mon <- 
              (Some ({mon = m; can_use_moves = true; 
                       will_attack_self = false}))

  let add_reserve_steammon s c m = 
    let player = match c with
    | Red -> s.red 
    | Blue -> s.blue in
    match player.steammons with
    | Some tbl -> Table.add tbl m.species m 
    | None -> 
        let new_reserve_pool = Table.create 1 in
        Table.add new_reserve_pool m.species m;
        player.steammons <- (Some new_reserve_pool)

  let remove_reserve_steammon s c m = 
    let player = match c with
    | Red -> s.red 
    | Blue -> s.blue in
    match player.steammons with
    | Some tbl -> Table.remove tbl m.species 
    | None -> failwith "Reserve pool empty."

  let swap_active_steammon s c m = 
    let player = match c with
    | Red -> s.red
    | Blue -> s.blue in
    match player.active_mon with
    | None -> 
        remove_reserve_steammon s c m;
        player.active_mon <- (Some ({mon = m; can_use_moves = true; 
				     will_attack_self = false;}))
    | Some active_mon -> 
        remove_reserve_steammon s c m;
        add_reserve_steammon s c active_mon.mon;
        active_mon.mon <- m


  let set_turn s c = 
      s.turn <- c


  (* Comparing the constructors for the actions to determine 
   * whether the expected action matches the responded action 
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
    | _ -> false *)
end


open Definitions
open Util
open Constants
open Netgraphics

(* You have to implement this. Change it from int to yout own state type*)
module GameState = State.GameState
type game = GameState.state

(*
 *Internally keeping track of a game instance to handle translating
 *between game_status_data and game types.
 *)
let game_instance = ref (GameState.initial_state ()) 

let opp_color c =
  match c with
  | Red -> Blue
  | Blue -> Red 

let valid_steammon s reserve_pool : bool = 
  (Table.mem reserve_pool s) && 
    ((Table.find reserve_pool s).curr_hp > 0)

let game_datafication (g:game) : game_status_data =
  let r_mons = GameState.get_steammon_list g Red in
  let r_inv = GameState.get_inv g Red in
  let r_creds = GameState.get_creds g Red in
  let r_team = (r_mons, r_inv, r_creds) in
  let b_mons = GameState.get_steammon_list g Blue in
  let b_inv = GameState.get_inv g Blue in
  let b_creds = GameState.get_creds g Blue in
  let b_team = (b_mons, b_inv, b_creds) in
  (r_team, b_team)
	
let game_from_data (game_data:game_status_data) : game = 
  let (r_team, b_team) = game_data in
  let (r_mons, r_inv, r_creds) = r_team in
  let (b_mons, b_inv, b_creds) = b_team in
  (*GameState.set_steammons !game_instance Red r_mons;*)
  GameState.set_inv !game_instance Red r_inv;
  GameState.set_creds !game_instance Red r_creds;
  (*GameState.set_steammons !game_instance Blue b_mons;*)
  GameState.set_inv !game_instance Blue b_inv;
  GameState.set_creds !game_instance Blue b_creds;
  !game_instance

let team_phase g rc bc = 
  let (red_name, blue_name) = match (rc, bc) with
  (*Initial team name response to update the GUI *)
  | (Action (SendTeamName r_name), Action (SendTeamName b_name)) ->
      (r_name, b_name)
  | (_ , Action (SendTeamName b_name)) ->
      ("Red", b_name)
  | (Action (SendTeamName r_name), _) ->
      (r_name, "Blue")
  | (_,_) -> 
      ("Red", "Blue") in
  Netgraphics.send_update (InitGraphics (red_name, blue_name));
  let draft_pick = Random.int 2 in
  let r_pick_req = 
    (if draft_pick = 0 then
      Some (Request (PickRequest (Red, (game_datafication g), 
        (GameState.get_move_list g), (GameState.get_base_mons g))))
    else 
      None) in
  let b_pick_req = 
    (if draft_pick = 1 then
      Some (Request (PickRequest (Blue, (game_datafication g), 
        (GameState.get_move_list g), (GameState.get_base_mons g))))
    else
      None) in
  GameState.set_phase g GameState.Draft;
  (None, (game_datafication g), r_pick_req, b_pick_req)

let draft_phase g ra ba = failwith "Implement draft_phase"

let stock_inventories g rc bc =
  let cost_lst = [cCOST_ETHER; cCOST_MAXPOTION; cCOST_FULLHEAL; cCOST_REVIVE; 
                  cCOST_XATTACK; cCOST_XDEFEND; cCOST_XSPEED] in
  let default_inv = [cNUM_ETHER; cNUM_MAX_POTION; cNUM_REVIVE; cNUM_FULL_HEAL; 
                     cNUM_XATTACK; cNUM_XDEFENSE; cNUM_XSPEED] in
  let stock_inventory_of (c: color) (inv: inventory) = 
    let cost = List.fold_left2 (fun worth item total -> worth*item + total) 0 cost_lst inv in
    if cost > cINITIAL_CASH then 
      GameState.set_inv g c default_inv
    else
      GameState.set_inv g c inv in
  let error_wrapper (c: color) = function
    | Action (PickInventory inv) -> stock_inventory_of c inv
    | DoNothing -> stock_inventory_of c default_inv
    | _ -> failwith "Neither PickInventory nor DoNothing" in
  error_wrapper Red rc;
  error_wrapper Blue bc;
  GameState.set_phase g GameState.Battle;
  (None, game_datafication g, Some (Request (StarterRequest (game_datafication g))), Some (Request (StarterRequest (game_datafication g))))


(****** STARTER PHASE BEGIN ******)

(*Tries to get an arbitrary_starter steammon, if all steammons have*)
(*fainted, check whether all the opposing steammons have fainted.*)
(*If so, game is a tie. If not, the opposing team wins. If there is*)
(*a steammon on the team that has not fainted, return that steammon.*)
let arbitrary_starter g c : steammon = 
  let steammon_list = GameState.get_steammon_list g c in
  let rec get_steammon lst = 
    match lst with
    | [] -> failwith "No steammons with player"
    | h::t -> 
        if h.curr_hp > 0 
        then h
        else get_steammon t in
  get_steammon steammon_list

(*Sets the active steammon from reserve_pool if the active steammon*)
(*faints or has not been chosen yet. If all reserve steammons are 
 *fainted, a game_result is set.*)
let battle_starter g rc bc : game_output = 
  let r_reserve_pool = GameState.get_reserve_pool g Red in
  let b_reserve_pool = GameState.get_reserve_pool g Blue in
  (match rc with
  | Action (SelectStarter rs) when (valid_steammon rs r_reserve_pool) ->
      let mon = Table.find r_reserve_pool rs in
      GameState.swap_active_steammon g Red mon
  | _ -> 
      let r_mon = arbitrary_starter g Red in
      GameState.swap_active_steammon g Red r_mon);
  (match bc with
  | Action (SelectStarter bs) when (valid_steammon bs b_reserve_pool) ->
      let mon = Table.find b_reserve_pool bs in
      GameState.swap_active_steammon g Blue mon
  | _ -> 
      let b_mon = arbitrary_starter g Blue in
      GameState.swap_active_steammon g Blue b_mon);
  let game_state = game_datafication g in
  (None, game_state, Some (Request (ActionRequest game_state)), 
    Some (Request (ActionRequest game_state)))

(****** STARTER PHASE END ******)


(****** BATTLE PHASE BEGIN *****)


(* status effects are only applied to the active Steammon *)
let handle_beginning_status (g: game) (mon: steammon) (team: color): unit = 
  let fate = Random.int 100 in
  let fate2 = Random.int 100 in
  match mon.status with
  | None -> ()
  | Some Paralyzed -> if fate < cPARALYSIS_CHANCE then 
			GameState.set_can_use_moves g team false
		      else (GameState.set_eff_speed g team mon 
			     ((GameState.get_eff_speed g team) / cPARALYSIS_SLOW))
  | Some Asleep -> if fate < cWAKE_UP_CHANCE then GameState.set_status g team mon None
		     else GameState.set_can_use_moves g team false
  | Some Frozen -> if fate < cDEFROST_CHANCE then GameState.set_status g team mon None
		     else GameState.set_can_use_moves g team false
  | Some Confused -> if fate < cSNAP_OUT_OF_CONFUSION then 
		       GameState.set_status g team mon None
		     else if fate2 < cSELF_ATTACK_CHANCE then 
		       GameState.set_will_attack_self g team true
		     else ()
  | Some Poisoned -> ()
  | Some Burned -> () (* burn weakness should be checked by seeing if 
                         status == burned when calculating damage *)

(*Need to check still alive after taking poison and burn damage *)
let handle_end_status g mon team : unit =
  match mon.status with 
  | None -> ()
  | Some Asleep -> GameState.set_can_use_moves g team true
  | Some Frozen -> GameState.set_can_use_moves g team true
  | Some Confused -> GameState.set_will_attack_self g team false
  | Some Paralyzed -> if GameState.get_can_use_moves g team = true then
			GameState.set_eff_speed g team mon 
			   (GameState.get_eff_speed g team * cPARALYSIS_SLOW)
		      else GameState.set_can_use_moves g team true
  | Some Poisoned -> GameState.set_hp g team mon (int_of_float ((
		       float_of_int (GameState.get_curr_hp g team)) -. 
		       ((float_of_int(GameState.get_max_hp g team)) *. cPOISON_DAMAGE)))
  | Some Burned -> GameState.set_hp g team mon (int_of_float ((
		       float_of_int (GameState.get_curr_hp g team)) -. 
		       ((float_of_int(GameState.get_max_hp g team)) *. cBURN_DAMAGE)))

(*Use item. *)
let use_item (g: game) (c: color) (i: item) (mon_string: string) =
  let inv = GameState.get_inv g c in
  let compare_active_mon = function
    | Some smon -> (smon.species = mon_string)
    | None -> failwith "use item shouldn't recieve None for active teammon" in
  let is_active_mon = compare_active_mon (GameState.get_active_mon g c) in
  let steammon_list = GameState.get_steammon_list g c in
  let rec get_target_mon (mon_lst: steammon list) (name: string) : steammon option =
    match mon_lst with
    | hd::tl -> if (hd.species = name) then (Some hd) else get_target_mon tl name
    | _ -> None in
  match (get_target_mon steammon_list mon_string) with
  (*item used on a mon that isn't on the team.
    treat as missing message--no changes.*)
  | None -> () 
  (*item used on a mon that is on the team.*)
  | Some target_mon ->
  (*match item*)
    match i with 

    | Ether -> let new_inv = List.mapi (fun i a -> if i = 0 then a else (a - 1)) inv in
      GameState.set_inv g c new_inv;
      if ((target_mon.curr_hp > 0) && ((List.nth inv 0) > 0)) then

        let use_ether move = 
          let new_pp = max (move.pp_remaining + 5) move.max_pp in
          {name = move.name;
          element = move.element;
          target = move.target;
          max_pp = move.max_pp;
          pp_remaining = new_pp;
          power = move.power;
          accuracy = move.accuracy;
          effects = move.effects} in 

        let new_ether_mon mon = 
          { species = mon.species; 
            curr_hp = mon.curr_hp; 
            max_hp = mon.max_hp;
            first_type = mon.first_type;
            second_type = mon.second_type;
            first_move = (use_ether mon.first_move);
            second_move = (use_ether mon.second_move);
            third_move = (use_ether mon.third_move);
            fourth_move = (use_ether mon.fourth_move);
            attack = mon.attack;
            spl_attack = mon.spl_attack;
            defense = mon.defense;
            spl_defense = mon.spl_defense;
            speed = mon.speed;
            status = mon.status;
            mods = mon.mods;
            cost = mon.cost } in
        let new_mon = new_ether_mon target_mon in
        Netgraphics.add_update(Item("Ether", RestoredPP 5 , c, mon_string));
        if is_active_mon then GameState.set_active_mon g c (Some new_mon)
        else let res_pool = GameState.get_reserve_pool g c in
          Table.replace res_pool mon_string new_mon
      else ()

    | MaxPotion -> let new_inv = List.mapi (fun i a -> if i = 1 then a else (a - 1)) inv in
      GameState.set_inv g c new_inv;
     (*not fainted. can use max potion*)
      if ((target_mon.curr_hp > 0) && ((List.nth inv 1) > 0)) then
        let new_max_potion_mon mon = 
          (*send CHANGE in health*)
          Netgraphics.add_update(Item("MaxPotion", Recovered (mon.max_hp - mon.curr_hp) , c, mon_string));
          Netgraphics.add_update(UpdateSteammon(mon.species, mon.max_hp, mon.max_hp, c));
          { species = mon.species; 
            curr_hp = mon.max_hp; 
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
        let new_mon = new_max_potion_mon target_mon in
        if is_active_mon then GameState.set_active_mon g c (Some new_mon)
        else let res_pool = GameState.get_reserve_pool g c in
          Table.replace res_pool mon_string new_mon
      (*fainted mon. can't use max potion*)
      else ()

    | Revive -> let new_inv = List.mapi (fun i a -> if i = 2 then a else (a - 1)) inv in
      GameState.set_inv g c new_inv;
     (*fainted. can use revive*)
      if ((target_mon.curr_hp <= 0) && ((List.nth inv 2) > 0)) then
        let new_revive_mon mon = 
          let new_hp = mon.max_hp/2 in
          (*send CHANGE in health*)
          Netgraphics.add_update(Item("Revive", Recovered new_hp, c, mon_string));
          Netgraphics.add_update(UpdateSteammon(mon.species, new_hp, mon.max_hp, c));
          { species = mon.species; 
            curr_hp = new_hp; 
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
        let new_mon = new_revive_mon target_mon in
        if is_active_mon then GameState.set_active_mon g c (Some new_mon)
        else let res_pool = GameState.get_reserve_pool g c in
          Table.replace res_pool mon_string new_mon
      (*not fainted. can't use revive*)
      else ()


  | FullHeal -> let new_inv = List.mapi (fun i a -> if i = 3 then a else (a - 1))  inv in
    GameState.set_inv g c new_inv;
    if ((target_mon.curr_hp > 0) && ((List.nth inv 3) > 0)) then
      (match target_mon.status with
      (*no status to heal from. do nothing*)
      | None -> ()
      (*has an inflicted status*)
      | Some inflicted_status ->
          Netgraphics.add_update(Item("FullHeal", HealedStatus inflicted_status, c, mon_string));
          let new_fullheal_mon mon = 
          { species = mon.species; 
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
            status = None;
            mods = mon.mods;
            cost = mon.cost } in
        let new_mon = new_fullheal_mon target_mon in
        if is_active_mon then GameState.set_active_mon g c (Some new_mon)
        else let res_pool = GameState.get_reserve_pool g c in
          Table.replace res_pool mon_string new_mon)
      else ()

  | XAttack -> let new_inv = List.mapi (fun i a -> if i = 4 then a else (a - 1)) inv in
    GameState.set_inv g c new_inv;
    if ((target_mon.curr_hp > 0) && ((List.nth inv 4) > 0)) then
      let new_xattack_mon mon = 
        let new_mods mods = 
          let new_attack_mod = max mods.attack_mod 6 in
          Netgraphics.add_update(Item("XAttack", StatModified (Atk, (new_attack_mod - mods.attack_mod)), c, mon_string));
          { attack_mod = new_attack_mod;
          defense_mod = mods.defense_mod;
          spl_attack_mod = mods.spl_attack_mod;
          spl_defense_mod = mods.spl_defense_mod;
          speed_mod = mods.speed_mod} in
      { species = mon.species; 
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
        status = mon.status;
        mods = new_mods mon.mods;
        cost = mon.cost } in
      let new_mon = new_xattack_mon target_mon in
      if is_active_mon then GameState.set_active_mon g c (Some new_mon)
      else let res_pool = GameState.get_reserve_pool g c in
        Table.replace res_pool mon_string new_mon
    else ()

  | XDefense -> let new_inv = List.mapi (fun i a -> if i = 5 then a else (a - 1)) inv in
    GameState.set_inv g c new_inv;
    if ((target_mon.curr_hp > 0) && ((List.nth inv 5) > 0)) then
      let new_xdefense_mon mon = 
        let new_mods mods = 
          let new_defense_mod = max mods.defense_mod 6 in
          Netgraphics.add_update(Item("XDefense", StatModified (Def, (new_defense_mod - mods.defense_mod)), c, mon_string));
          { attack_mod = mods.attack_mod;
          defense_mod = new_defense_mod;
          spl_attack_mod = mods.spl_attack_mod;
          spl_defense_mod = mods.spl_defense_mod;
          speed_mod = mods.speed_mod} in
      { species = mon.species; 
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
        status = mon.status;
        mods = new_mods mon.mods;
        cost = mon.cost } in
      let new_mon = new_xdefense_mon target_mon in
      if is_active_mon then GameState.set_active_mon g c (Some new_mon)
      else let res_pool = GameState.get_reserve_pool g c in
        Table.replace res_pool mon_string new_mon
    else ()

  | XSpeed -> let new_inv = List.mapi (fun i a -> if i = 6 then a else (a - 1)) inv in
    GameState.set_inv g c new_inv;
    if ((target_mon.curr_hp > 0) && ((List.nth inv 6) > 0)) then
      let new_xspeed_mon mon = 
        let new_mods mods = 
          let new_speed_mod = max mods.speed_mod 6 in
          Netgraphics.add_update(Item("XSpeed", StatModified (Spe, (new_speed_mod - mods.speed_mod)), c, mon_string));
          { attack_mod = mods.attack_mod;
          defense_mod = mods.defense_mod;
          spl_attack_mod = mods.spl_attack_mod;
          spl_defense_mod = mods.spl_defense_mod;
          speed_mod = new_speed_mod} in
      { species = mon.species; 
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
        status = mon.status;
        mods = new_mods mon.mods;
        cost = mon.cost } in
      let new_mon = new_xspeed_mon target_mon in
      if is_active_mon then GameState.set_active_mon g c (Some new_mon)
      else let res_pool = GameState.get_reserve_pool g c in
        Table.replace res_pool mon_string new_mon
    else ()


let use_move g c move : game_result option = failwith "Implement move use"
let switch_steammon g c mon : game_result option = failwith "Implement steammon switching"
let switch_active g c mon : game_result option = failwith "Implement active steammon switch"
let switch_active_arbitrary g c : game_result option = failwith "Implement active steammon switch"

(* steammon if there exists at least one steammon that has not fainted,
 * None otherwise *)
let rec faint_check (lst:steammon list) : steammon option =
  match lst with
  | [] -> failwith "No steammons in list"
  | h::[] when h.curr_hp > 0 -> Some h
  | h::[] -> None
  | h::t when h.curr_hp > 0 -> Some h
  | h::t -> faint_check t

(* Checks whether the active steammon fainted by an action immediately
 * prior. (Status effect, Move, Inv use etc) *)
let active_faint_check g c : bool = 
  match (GameState.get_active_mon g c) with
  | None -> false
  | Some s -> s.curr_hp <= 0

let battle_action g c comm : game_result option = 
  match (GameState.get_active_mon g c) with
  (* Active steammon has fainted, require SelectStarter *)
  | None -> 
      (match comm with 
      | Action (SelectStarter s) -> switch_active g c s 
      | _ -> switch_active_arbitrary g c)
  | Some _ ->
      (match comm with
      | Action (UseItem (i, s)) -> (use_item g c i s); None
      | Action (UseMove s) -> use_move g c s
      | Action (SwitchSteammon s) -> switch_steammon g c s
      | Action (SelectStarter s) -> switch_active g c s
      | _ -> None)

let battle_phase g rc bc : game_output = 
  (* Apply the status effects, handle the outstanding actions and
   * then send out requests depending on whose turn it is *)
  (match (GameState.get_active_mon g Red) with
  | Some mon -> handle_beginning_status g mon Red
  | None -> () );
  (match (GameState.get_active_mon g Blue) with
  | Some mon -> handle_beginning_status g mon Blue
  | None -> () );
  
  (* IMPORTANT< If a steammon faints during handling status, 
   * we have to bypass resolving actions as per 4.4.4 *)
  let (faster_action, slower_action, faster_color) = 
    if (GameState.get_eff_speed g Red) > (GameState.get_eff_speed g Blue) 
    then (rc, bc, Red) 
    else (bc, rc, Blue) in

  let result = battle_action g faster_color faster_action in
  match result with
  | Some res -> (Some res, (game_datafication g), None, None)
  | None -> 
      let result2 = battle_action g (opp_color faster_color) slower_action in
      match result2 with
      | Some res -> (Some res, (game_datafication g), None, None)
      | None ->
          (match (GameState.get_active_mon g Red) with
          | Some mon -> handle_end_status g mon Red
          | None -> () );
          (match (GameState.get_active_mon g Blue) with
          | Some mon -> handle_end_status g mon Blue
          | None -> () );
          let r_req = 
            if (active_faint_check g Red) then
              Request (StarterRequest (game_datafication g))
            else 
              Request (ActionRequest (game_datafication g)) in
          let b_req = 
            if (active_faint_check g Blue) then
              Request (StarterRequest (game_datafication g))
            else 
              Request (ActionRequest (game_datafication g)) in
          (None, (game_datafication g), Some r_req, Some b_req)

(****** BATTLE PHASE END   *****)


let handle_step (g:game) (rc:command) (bc:command) : game_output =
  (* Handle status effects that occur at end of turn *)
  let current_phase = GameState.get_phase g in
  match current_phase with
  | GameState.TeamName -> team_phase g rc bc
  | GameState.Draft -> draft_phase g rc bc
  | GameState.Inventory -> stock_inventories g rc bc
  | GameState.Starter -> battle_starter g rc bc
  | GameState.Battle -> battle_phase g rc bc	      

let init_game () : game * request * request * move list * steammon list =
  (* Creating a blank state for the beginning of the game *)
  let init_state = !game_instance in

  (* Loading moves list and Steammon list *)
  let mvs = GameState.get_move_list init_state in
  let mons = GameState.get_base_mons init_state in

  (init_state, TeamNameRequest, TeamNameRequest, mvs, mons)
